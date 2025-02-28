#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QFile>
#include <QTextStream>
#include <QDebug>
#include <QMessageBox>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow),
      adminCenter(nullptr),
      subscribeForm(nullptr),
      forgot_credentials(nullptr)
{
    ui->setupUi(this);

    setCentralWidget(ui->centralwidget);
    ui->centralwidget->setLayout(ui->gridLayout);

    //control existence users.txt
    QFile file("users.txt");
    if (!file.exists()){
        if (file.open(QIODevice::WriteOnly | QIODevice::Text)){
            QTextStream out(&file);
                out << "Elenco utenti registrati:\n";
            file.close();
            qDebug() << "File creato.";
        } else {
            qDebug() << "Can't create file.";
        }
    }
    connect(ui->loginButton, &QPushButton::clicked, this, &MainWindow::onLoginClicked);
    connect(ui->passwordTextBox, &QLineEdit::returnPressed, this, &MainWindow::onLoginClicked);
    connect(ui->RegisterButton, &QPushButton::clicked, this, &MainWindow::subscribeClicked);
    connect(ui->forgetButton, &QPushButton::clicked, this, &MainWindow::onForgotClicked);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::subscribeClicked(){
    if (!subscribeForm){
        subscribeForm = new subscribe(this);
    }
    subscribeForm -> show();
    subscribeForm -> raise();
    subscribeForm -> activateWindow();
    this->hide();
}

//admin login
bool MainWindow::verifyAdminLogin(QString email, QString password){
 return (email == "admin@progcpp.com" && password == "admin");
}

void MainWindow::openAdminCenter(){
    if (!adminCenter){
        adminCenter = new AdminCenter(this);
    }
    adminCenter -> show();
    adminCenter -> raise();
    adminCenter -> activateWindow();
    this->hide();
}

void MainWindow::onLoginClicked() {
    QString email = ui->emailTextBox->text();
    QString password = ui->passwordTextBox->text();

    if (email.isEmpty() || password.isEmpty()){
        QMessageBox::warning(this, "Errore", "Per favore, inserisci email e password");
        return;
    }

    if (verifyAdminLogin(email, password)){
        openAdminCenter(); return;
    }

    if (verifyLogin(email, password)){
        QMessageBox::information(this, "Accesso riuscito", "Benvenuto!");
        this->hide();
    } else {
        QMessageBox::critical(this, "Errore", "Email o password errati.");
    }
}

bool MainWindow::verifyLogin(QString email, QString password){
    QFile file("users.txt");
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)){
        qDebug() << "Impossibile aprire il file users.txt";
        return false;
    }

    QTextStream in(&file);
    QString storedEmail, storedPassword;

    while (!in.atEnd()) {
        QString line = in.readLine().trimmed();

        if (line.startsWith("Email:")){
            storedEmail = line.section(':', 1).trimmed();
        } else if (line.startsWith("Password:")){
            storedPassword = line.section(':',1).trimmed();
        } else if (line.startsWith("------------------------")){
            if (!storedEmail.isEmpty() && !storedPassword.isEmpty()) {
                if (email == storedEmail && password == storedPassword){
                    file.close(); return true;
                }
            }
            storedEmail.clear(); storedPassword.clear();
        }
    }
    file.close(); return false;
}

void MainWindow::onForgotClicked(){
    if (!forgot_credentials){
        forgot_credentials = new forgotCredentials(this);
    }
    forgot_credentials -> show();
    forgot_credentials -> raise();
    forgot_credentials -> activateWindow();
    this->hide();
}
