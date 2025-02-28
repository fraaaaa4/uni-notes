#include "admincenter.h"
#include "ui_admincenter.h"
#include "mainwindow.h"
#include <QCloseEvent>
#include <QFile>
#include <QTextStream>
#include <QList>
#include <QDate>
#include <QDebug>
#include <QtCharts/QChartView>
#include <QtCharts/QPieSeries>
#include <QtCharts/QChart>

struct User {
    QString nome; QString cognome; QString email; QString password;
    QString dataNascita; QString genere;
};

QList<User> AdminCenter::readUsersFromFile(){
    QList<User> users; QFile file("users.txt");

    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)){
        qWarning() << "Impossibile aprire il file users.txt";
        return users;
    }

    QTextStream in(&file); User user;
    while (!in.atEnd()){
        QString line = in.readLine();
        if (line.startsWith("Nome: ")) user.nome = line.mid(6);
        else if (line.startsWith("Cognome: ")) user.cognome = line.mid(9);
        else if (line.startsWith("Email: ")) user.email = line.mid(7);
        else if (line.startsWith("Password: ")) user.password = line.mid(10);
        else if (line.startsWith("Data di nascita: ")) user.dataNascita = line.mid(18);
        else if (line.startsWith("Genere: ")) user.genere = line.mid(8);
        else if (line == "------------------------") {
            users.append(user);
            user = User(); // Reset user data
    }
}

    file.close();
    return users;
}

void AdminCenter::countStats(const QList<User>& users, int& numMen, int& numWomen, int ageGroups[5]) {
    QDate currentDate = QDate::currentDate();

    numMen = 0;
    numWomen = 0;
    for (int i = 0; i < users.size(); ++i) {
        const User& user = users[i];

        if (user.genere == "Uomo") {
            numMen++;
        } else if (user.genere == "Donna") {
            numWomen++;
        }

        QDate birthDate = QDate::fromString(user.dataNascita, "dd/MM/yyyy");
        int age = currentDate.year() - birthDate.year();
        if (birthDate.month() < currentDate.month() || (birthDate.month() == currentDate.month() && birthDate.day() <= currentDate.day())) {
            age--; }

        if (age >= 18 && age <= 26) {
            ageGroups[0]++;
        } else if (age >= 27 && age <= 35) {
            ageGroups[1]++;
        } else if (age >= 36 && age <= 44) {
            ageGroups[2]++;
        } else if (age >= 45 && age <= 53) {
            ageGroups[3]++;
        } else if (age >= 54) {
            ageGroups[4]++;
        }
    }
}

void AdminCenter::createGenderChart(int numMen, int numWomen){
    QtCharts::QPieSeries* series = new QtCharts::QPieSeries();
    series->append("Uomini", numMen);
    series->append("Donne", numWomen);

    QtCharts::QChart* chart = new QtCharts::QChart();
    chart->addSeries(series);
    chart->setTitle("Percentuale di uomini e donne");

    QtCharts::QChartView* chartView = new QtCharts::QChartView(chart);
    chartView->setRenderHint(QPainter::Antialiasing);
    ui->gridLayout->addWidget(chartView);
}

void AdminCenter::createAgeGroupChart(int ageGroups[5]){
    QtCharts::QPieSeries* series = new QtCharts::QPieSeries();
    series->append("18-26", ageGroups[0]);
    series->append("27-35", ageGroups[1]);
    series->append("36-44", ageGroups[2]);
    series->append("45-53", ageGroups[3]);
    series->append("54+", ageGroups[4]);

    QtCharts::QChart* chart = new QtCharts::QChart();
    chart->addSeries(series);
    chart->setTitle("Distribuzione età");
    QtCharts::QChartView* chartView = new QtCharts::QChartView(chart);
    chartView->setRenderHint(QPainter::Antialiasing);
    ui->gridLayout->addWidget(chartView);
}

void AdminCenter::populateUserTable(const QList<User>& users){
    ui->userTable->setRowCount(users.size());
    for (int i = 0; i < users.size(); ++i){
        const User& user = users[i];
        ui->userTable->setItem(i, 0, new QTableWidgetItem(user.nome));
                ui->userTable->setItem(i, 1, new QTableWidgetItem(user.cognome));
                ui->userTable->setItem(i, 2, new QTableWidgetItem(user.email));
                ui->userTable->setItem(i, 3, new QTableWidgetItem(user.password));
                ui->userTable->setItem(i, 4, new QTableWidgetItem(user.dataNascita));
                ui->userTable->setItem(i, 5, new QTableWidgetItem(user.genere));
    }
}

AdminCenter::AdminCenter(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AdminCenter)
{
    ui->setupUi(this);

    setLayout(ui->gridLayout_2);

    QList<User> users = readUsersFromFile();
    int numMen = 0, numWomen = 0, ageGroups[5] = {0};
    countStats(users, numMen, numWomen, ageGroups);
    createGenderChart(numMen, numWomen);
    createAgeGroupChart(ageGroups);
    populateUserTable(users);
}

AdminCenter::~AdminCenter()
{
    delete ui;
}

void AdminCenter::closeEvent(QCloseEvent *event)
{
    if (parentWidget())
        parentWidget()->show();
    event->accept();
}
