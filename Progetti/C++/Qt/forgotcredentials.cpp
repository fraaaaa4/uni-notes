#include "forgotcredentials.h"
#include "ui_forgotcredentials.h"
#include "mainwindow.h"
#include <QCloseEvent>
#include <QFile>
#include <QTextStream>
#include <QMessageBox>
#include <QDebug>

forgotCredentials::forgotCredentials(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::forgotCredentials)
{
    ui->setupUi(this);
    this->setFixedSize(this->size());
    connect(ui->loginButton, &QPushButton::clicked, this, &forgotCredentials::recoverPassword);
}

forgotCredentials::~forgotCredentials()
{
    delete ui;
}

void forgotCredentials::closeEvent(QCloseEvent *event)
{
    if (parentWidget())
        parentWidget()->show();
    event->accept();
}

void forgotCredentials::recoverPassword(){
    QString emailInserita = ui->nameTextBox->text().trimmed();

    if (emailInserita.isEmpty()){
        QMessageBox::warning(this, "Errore", "Inserisci un indirizzo email.");
        return;
    }

    QFile file("users.txt");
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QMessageBox::warning(this, "Errore", "Impossibile aprire il file users.txt.");
        return;
    }

    QTextStream in(&file);
    QString line;
    bool found = false;
    QString password;

    while (!in.atEnd()){
        line = in.readLine();
        if (line.startsWith("Email:")){
            QString emailSalvata = line.mid(7).trimmed();
            if (emailSalvata == emailInserita) found = true;
        } else if (found && line.startsWith("Password:")){
            password = line.mid(10).trimmed();
            break;
        }
    }

    file.close();

    if (found) {
        QMessageBox::information(this, "Password trovata", "Email con procedura di ripristino inviata, controlla la tua casella postale.");
    } else {
        QMessageBox::warning(this, "Errore", "Non c'è nessun utente registrato con questo indirizzo email.");
    }
}
