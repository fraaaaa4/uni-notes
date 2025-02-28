#include "subscribe.h"
#include "ui_subscribe.h"
#include "mainwindow.h"
#include <QCloseEvent>
#include <QFile>
#include <QTextStream>
#include <QMessageBox>
#include <QDebug>

subscribe::subscribe(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::subscribe)
{
    ui->setupUi(this);

    setLayout(ui->gridLayout);

    connect(ui->loginButton, &QPushButton::clicked, this, &subscribe::saveUserToFile);
}

subscribe::~subscribe()
{
    delete ui;
}

void subscribe::closeEvent(QCloseEvent *event)
{
    if (parentWidget())
        parentWidget()->show();
    event->accept();
}

bool checkNotAdmin(QString email){
    return (email != "admin@progcpp.com");
}

void subscribe::saveUserToFile() {
    QString nome = ui->nameTextBox->text();
    QString cognome = ui->surnameTextBox->text();
    QString email = ui->emailTextBox->text().trimmed();
    QString password = ui->passwordTextBox->text();
    QString dataNascita = ui->bornDateEdit->text();
    QString genere = ui->womenRadioButton->isChecked() ? "Donna" : "Uomo";

    if (nome.isEmpty() || cognome.isEmpty() || email.isEmpty() || password.isEmpty()){
        QMessageBox::warning(this, "Errore", "Compila tutti i campi!");
        return;
    }

    QDate dataNascitaDate = QDate::fromString(dataNascita, "dd/MM/yyyy");
    if (!dataNascitaDate.isValid()){
        QMessageBox::warning(this, "Errore", "La data di nascita non è valida.");
        return;
    }

    QDate oggi = QDate::currentDate();
    int age = oggi.year() - dataNascitaDate.year();
    if (oggi.month() < dataNascitaDate.month() || (oggi.month() == dataNascitaDate.month() && oggi.day() < dataNascitaDate.day()))
       age--;

    if (age < 18) {
        QMessageBox::warning(this, "Errore", "Devi essere maggiorenne per registrarti.");
        return;
    }

    QFile file("users.txt");
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)){
        QMessageBox::warning(this, "Errore", "Impossibile aprire il file users.txt per la lettura.");
        return;
    }

    QTextStream in(&file);
    QString line;
    bool emailExists = false;
    while (!in.atEnd()) {
        line = in.readLine();
        if (line.contains("Email: " + email)) {
            emailExists = true;
            break;
        }
    }
    file.close();

    if (emailExists) {
        QMessageBox::warning(this, "Errore", "Questa email è già registrata!");
        return;
    }

    if (!file.open(QIODevice::Append | QIODevice::Text)){
        QMessageBox::warning(this, "Errore", "Impossibile aprire il file users.txt per scrivere.");
        return;
    }

    QTextStream out(&file);
    out << "Nome: " << nome << "\n";
    out << "Cognome: " << cognome << "\n";
    out << "Email: " << email << "\n";
    out << "Password: " << password << "\n";
    out << "Data di nascita: " << dataNascita << "\n";
    out << "Genere: " << genere << "\n";
    out << "------------------------\n";

    file.close();
    QMessageBox::information(this, "Successo", "Registrazione completata!");
    this->close();
}
