
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <ctype.h>
#include <signal.h>
#include <readline/readline.h>
#include <readline/history.h>

#define MAX_LINE 4096
#define MAX_ARGS 256
#define MAX_PATH 512
#define MAX_PROMPT 32
#define MAX_VARIABLES 64
#define _XOPEN_SOURCE 700
#define HISTORY_SIZE 10

char _path[MAX_PATH] = "/bin/:/usr/bin";
pid_t current_pid = 0;
pid_t shell_pid;
char prompt_string[MAX_PROMPT] = "\0";

struct Variable {
  char name[MAX_LINE];
  char value[MAX_LINE];
};
  
  struct Variable variables[MAX_VARIABLES];
size_t num_variables = 0;

void handle_sigint(int sig) {
    printf("\n");
    fflush(stdout);
}

void panic(const char* msg) {
  if (errno) {
    fprintf(stderr, "Panic: %s: %s\n\n", msg, strerror(errno));
  } else {
    fprintf(stderr, "Panic: %s\n\n", msg);
  }
  exit(EXIT_FAILURE);
}

void path_lookup(char* abs_path, const char* rel_path) {
  char* prefix;
  char buf[MAX_PATH];
  if(abs_path == NULL || rel_path == NULL)
    panic("path_lookup: parameter error");
  prefix = strtok(_path, ":");
  while(prefix != NULL){
    strcpy(buf, prefix);
    strcat(buf, rel_path);
    if(access(buf, X_OK) == 0){
      strcpy(abs_path, buf);
      return;
    }
    prefix = strtok(NULL, ":");
  }
  strcpy(abs_path, rel_path);
}

void set_path(const char* new_path){
  if (new_path != NULL){
    
#if USE_DEBUG_PRINTF
    printf("DEBUG: new_path: %s\n", new_path);
#endif
    
    int cur_pos = 0;
    while(new_path[cur_pos] != '\0') {
      cur_pos++;
      if(cur_pos >= MAX_PATH - 1 && new_path[cur_pos] != '\0') {
	fprintf(stderr, "Error: PATH string too long\n");
	return;
      }
    }
    if(cur_pos > 0)
      memcpy(_path, new_path, cur_pos + 1);
  }
  printf("%s\n", _path);
}

void exec_rel2abs(char** arg_list){
  if(arg_list[0][0] == '/'){
    execv(arg_list[0], arg_list);
  } else {
    //relative path
    char abs_path[MAX_PATH];
    path_lookup(abs_path, arg_list[0]);
    execv(abs_path, arg_list);
  }
}

void do_redir(const char* out_path, char** arg_list, const char* mode){
  if(out_path == NULL)
    panic("do_redir: no path");
  
  int pid = fork();
  if(pid > 0){
    int wpid = wait(NULL);
    if (wpid<0) panic("do_redir: wait");
  } else if(pid == 0){
    FILE* out = fopen(out_path, mode);
    if(out == NULL){
      perror(out_path);
      exit(EXIT_FAILURE);
    }
    dup2(fileno(out), 1); //1 = stdout
    exec_rel2abs(arg_list);
    perror(arg_list[0]);
    exit(EXIT_FAILURE);
  } else {
    panic("do_redir: fork");
  }
}

void do_pipe(size_t pipe_pos, char** arg_list){
  int pipefd[2];
  int pid;
  
  if(pipe(pipefd) <0) panic("do_pipe: pipe");
  //left side
  pid = fork();
  if(pid > 0){
    int wpid = wait(NULL);
    if(wpid < 0) panic("do_pipe: wait");
  } else if (pid == 0){
    close(pipefd[0]);
    dup2(pipefd[1], 1);
    close(pipefd[1]);
    exec_rel2abs(arg_list);
    perror(arg_list[0]);
    exit(EXIT_FAILURE);
  } else {
    panic("do_pipe: fork");
  }
  
  pid = fork();
  if (pid > 0) {
    close(pipefd[0]);
    close(pipefd[1]);
    int wpid = wait(NULL);
    if (wpid < 0) panic("do_pipe: wait");
  } else if(pid == 0) {
    close(pipefd[1]);
    dup2(pipefd[0],0);
    close(pipefd[0]);
    exec_rel2abs(arg_list + pipe_pos + 1);
    perror(arg_list[pipe_pos+1]);
    exit(EXIT_FAILURE);
  } else {
    panic("do_pipe: fork");
  }
}

void set_variable(const char *name, const char *value) {
  if (num_variables < MAX_VARIABLES) {
    strncpy(variables[num_variables].name, name, sizeof(variables[num_variables].name) -1);
    strncpy(variables[num_variables].value, value, sizeof(variables[num_variables].value) - 1);
    num_variables++;
  } else {
    printf("Too many variables, cannot set %s\n", name);
  }
}

const char *get_variable_value(const char *name) {
  for (size_t k = 0; k < num_variables; k++){
    if (strcmp(variables[k].name, name) == 0) {
      return variables[k].value;
    }
  }
  return "";
}

void parse_input(char* line, char** args, size_t* arg_count) {
    int i = 0;
    args[i] = strtok(line, " \n");
    while (args[i] != NULL) {
        (*arg_count)++;
        i++;
        args[i] = strtok(NULL, " ");
    }
}

void execute_command(char** args) {
    pid_t pid = fork();

    if (pid < 0) {
        perror("fork failed");
        exit(EXIT_FAILURE);
    }

    if (pid == 0) {
        signal(SIGINT, SIG_DFL);
        execvp(args[0], args);
        perror("exec failed");
        exit(EXIT_FAILURE);
    } else {
        int status;
        waitpid(pid, &status, 0);
    }
}

void expand_variables(char *input) {
  char expanded[MAX_LINE] = "";
  char *pos = input;
  char *start = NULL;

  while ((start = strchr(pos, '$')) != NULL){
    strncat(expanded, pos, start - pos);

    char *end = start + 1;
    while (*end && (*end == '_' || isalnum(*end))){
      end++;
    }

    char var_name[MAX_LINE];
    strncpy(var_name, start + 1, end - start - 1);
    var_name[end - start - 1] = '\0';

    const char *var_value = get_variable_value(var_name);
    strncat(expanded, var_value, MAX_LINE - strlen(expanded) - 1);

    // Aggiungi uno spazio solo se c'è un altro argomento dopo la variabile
    if (*end != '\0') {
      strncat(expanded, " ", MAX_LINE - strlen(expanded) - 1);
    }

    pos = end;
  }
  strncat(expanded, pos, MAX_LINE - strlen(expanded) - 1);
  strncpy(input, expanded, MAX_LINE - 1);
}

void print_help(){
    printf("dsh - A simple shell\n");
    printf("Commands:\n");
    printf("  exit\n");
    printf("  setpath <path>\n");
    printf("  set <variable> <value>\n");
    printf("  $<variable>\n");
    printf("  echo <variable>\n");
    printf("  <command> [args]\n");
    printf("The shell supports usage of special characters:\n");
    printf("  >, <, |, &\n");
    printf("You can exit from a running command with Ctrl-C\n");
}

size_t arg_count;

int main() {
    // Dichiarazioni di variabili
    char line[MAX_LINE];
    
    char *args[MAX_LINE / 2 + 1];
    int run_in_background = 0;

    // Gestione del segnale SIGINT
    signal(SIGINT, handle_sigint);

    while (1) {
      arg_count = 0;
        printf("dsh$ ");
        fgets(line, MAX_LINE, stdin);

        // Espansione delle variabili
        expand_variables(line);

        // Tokenizzazione con parse_input
        parse_input(line, args, &arg_count);

 if(strcmp(args[0], "exit")  == 0) {
  break;
}
    if(strcmp(args[0], "setpath") == 0) {
  set_path(args[1]);
  continue;
}

    if (strcmp(args[0], "set") == 0) {
      if (arg_count >= 0) {
        set_variable(args[1], args[2]);
      } else {
        printf("Usage: set <variable> <value>\n");
      }
      continue;
    }

if(strcmp(args[0], "help") == 0) {
  print_help();
  continue;
}

    if (args[0][0] == '$') {
      const char *variable_value = get_variable_value(&args[0][1]);
      if (variable_value[0] != '\0') {
        printf("%s\n", variable_value);
      } else {
        printf("Variable %s not set\n", &args[0][1]);
      }
      continue;
    }

    if(strcmp(args[0], "echo") == 0){
      for(size_t k = 1; k < arg_count; k++){
        if (args[k][0] == '$') {
          const char *variable_value = get_variable_value(&args[k][1]);
          printf("%s ", variable_value);
        } else {
          printf("%s ", args[k]);
        }
      }
      printf("\n");
      continue;
    }

        // Gestione dei caratteri speciali
        size_t special_char_pos = 0;
        int special_char_found = 0;
        for (size_t j = 0; args[j] != NULL; j++) {
          if (strcmp(args[j], ">") == 0 || strcmp(args[j], ">>") == 0 || strcmp(args[j], "|") == 0 || strcmp(args[j], "&") == 0) {
            special_char_pos = j;
            special_char_found = 1;
            break;
          }
        }
        if (special_char_found) {
          char* special_char = args[special_char_pos];
          char* out_path = args[special_char_pos + 1];
          args[special_char_pos] = NULL;
          char* args_copy[MAX_LINE/2 + 1];
          for (size_t i = 0; i < MAX_LINE/2 + 1; i++) {
            args_copy[i] = args[i];
          }
          if (strcmp(special_char, ">") == 0) {
            do_redir(out_path, args_copy, "w+");
          } else if (strcmp(special_char, ">>") == 0) {
            do_redir(out_path, args_copy, "a+");
          } else if (strcmp(special_char, "|") == 0) {
            do_pipe(special_char_pos, args_copy);
          } else if (strcmp(special_char, "&") == 0) {
            pid_t child_pid = fork();
            run_in_background = 1;
            if (child_pid == 0) {
              signal(SIGINT, SIG_DFL);
              execute_command(args_copy);
            } else if (child_pid > 0) {
              int status;
              if (!run_in_background) waitpid(child_pid, &status, 0);
              else perror("fork");
            }
            run_in_background = 0;
          }
        } else {
          signal(SIGINT, handle_sigint);
          execute_command(args);
        }
    }

    return 0;
}