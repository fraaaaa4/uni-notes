/* dsh.c 21 Gen 2024 */

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

char command_history[HISTORY_SIZE][MAX_LINE];
size_t history_index = 0;
size_t history_count = 0;

int prompt(char* buf, size_t buf_size, const char* prompt_string) {
  size_t cur = -1;
  
  printf("%s", prompt_string);
  if(fgets(buf, buf_size, stdin) == NULL) {
    return EOF;
  }
  do{
    cur++;
    if(buf[cur] == '\n') {
      buf[cur] = '\0';
      break;
    }
  }while(buf[cur] != '\0');
  return cur;
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

void handle_sigint(int sig) {
  if (getpid() == shell_pid) {
    printf("\n");
  } else {
    exit(EXIT_FAILURE);
  }
}

  void expand_variables(char *input) {
  char expanded[MAX_LINE];
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
  if(strcmp(var_name, "echo") == 0) {
  strncat(expanded, var_value, sizeof(expanded) - 1);
} else {
  strncat(expanded, var_value, sizeof(expanded) - 1);
  strncat(expanded, " ", sizeof(expanded) - 1);
}
  pos = end;
  strncat(expanded, pos, MAX_LINE - strlen(expanded) - 1);
  strncpy(input, expanded, MAX_LINE - 1);
}
}

void do_exec (char** arg_list) {
  int pid = fork();
  if(pid >0){
    // Nel processo padre, aspetta che il processo figlio termini
    int wpid = wait(NULL);
    if (wpid < 0) panic("wait");

    // Cambia il gruppo di processi in primo piano nel terminale al gruppo di processi del processo padre
    tcsetpgrp(STDIN_FILENO, getpgrp());
  } else if(pid == 0){
    // Nel processo figlio, crea un nuovo gruppo di processi
    setpgid(0, 0);

    // Cambia il gruppo di processi in primo piano nel terminale al gruppo di processi del processo figlio
    tcsetpgrp(STDIN_FILENO, getpgrp());

    // Ripristina l'azione predefinita per SIGINT
    signal(SIGINT, SIG_DFL);

    // Esegui il comando
    exec_rel2abs(arg_list);
    perror(arg_list[0]);
    exit(EXIT_FAILURE);
  } else {
    panic("do_exec: fork");
  }
}

int main(void) {
  //variable declarations
  char input_buffer[MAX_LINE];
  size_t arg_count;
  char* arg_list[MAX_ARGS];
  size_t i;
  int run_in_background = 0;
 
  shell_pid = getpid();
  sigaction(SIGINT, &(struct sigaction){ .sa_handler = handle_sigint }, NULL);

  if (isatty(0)){
  strcpy(prompt_string, "dsh$ \0");
}
  
  char* temp_buffer;
  while((temp_buffer = readline(prompt_string)) != NULL) {
    strncpy(input_buffer, temp_buffer, sizeof(input_buffer) - 1);
    add_history(input_buffer);
    free(temp_buffer);

  fflush(stdout);

input_buffer[strcspn(input_buffer, "\n")] = '\0';
    expand_variables(input_buffer);
  
    //tokenize
    arg_count = 0;
    arg_list[arg_count] = strtok(input_buffer, " ");
    if(arg_list[arg_count] == NULL) {
      // nothing specified at the command prompt
      continue;
    } else {
      do {
	arg_count++;
	if (arg_count > MAX_ARGS) break;
	arg_list[arg_count] = strtok(NULL, " ");
      } while (arg_list[arg_count] != NULL);
    }

#if USE_DEBUG_PRINTF
    printf("DEBUG: tokens:");
    for (i = 0; i < arg_count; i++){
      printf(" %s", arg_list[i]);
}
    puts("");
#endif
    if(strcmp(arg_list[0], "exit")  == 0) {
  break;
}
    if(strcmp(arg_list[0], "setpath") == 0) {
  set_path(arg_list[1]);
  continue;
}
    if(strcmp(arg_list[0], "set") == 0 && arg_count > 2 && arg_list[1][0] == '$'){
  set_variable(&arg_list[1][1], arg_list[2]);
  continue;
}
    if(arg_list[0][0] ==  '$') {
  const char *variable_value = get_variable_value(&arg_list[0][1]);
  printf("%s\n", variable_value);
  continue;
}
    if(strcmp(arg_list[0], "echo") == 0){
  for(size_t k = 1; k < arg_count; k++){
  const char *variable_value = get_variable_value(arg_list[k] + 1);
  if (variable_value[0] == '\0'){
  printf("%s ", arg_list[k]);
} else {
  printf("%s ", variable_value);
}
}
  printf("\n");
  continue;
}
    
    {
      //check for special characters
      size_t redir_pos = 0;
      size_t append_pos = 0;
      size_t pipe_pos = 0;
      size_t background_pos = 0;
      for (size_t j = 0; j < arg_count; j++){
	if(strcmp(arg_list[j],  ">") == 0){
	  redir_pos = j;
	  break;
	}
	if(strcmp(arg_list[j], ">>") == 0){
	  append_pos = j;
	  break;
	}
	if(strcmp(arg_list[j], "|") == 0) {
	  pipe_pos = j;
	  break;
	}
	if(strcmp(arg_list[j], "&") == 0){
	  background_pos = j;
	  break;
	}
      }
      //redirect
      if(redir_pos != 0){
	arg_list[redir_pos] = NULL;
	do_redir(arg_list[redir_pos + 1], arg_list, "w+");
      } else if (append_pos != 0){
	arg_list[append_pos] = NULL;
	do_redir(arg_list[append_pos+1], arg_list, "a+");
      } else if (pipe_pos != 0){
	arg_list[pipe_pos] = NULL;
	do_pipe(pipe_pos, arg_list);
      } else if (background_pos != 0){
	arg_list[background_pos] = NULL;
	pid_t child_pid = fork();
	run_in_background = 1;
	if (child_pid == 0){
    signal(SIGINT, SIG_DFL);
	  do_exec(arg_list);
	} else if (child_pid > 0){
  current_pid = child_pid; int status;
	  if(!run_in_background) waitpid(child_pid, &status, 0);
	  else  perror("fork");
	}
	run_in_background = 0;
      } else {
        signal(SIGINT, handle_sigint);
        do_exec(arg_list);
      }
      
    }
  exit(EXIT_SUCCESS);
}
}
