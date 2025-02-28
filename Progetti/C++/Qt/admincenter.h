#ifndef ADMINCENTER_H
#define ADMINCENTER_H

#include <QDialog>
#include <QtCharts/QChartView>
#include <QtCharts/QPieSeries>
#include <QtCharts/QChart>

class MainWindow;

struct User;

namespace Ui {
class AdminCenter;
}

class AdminCenter : public QDialog
{
    Q_OBJECT

public:
    explicit AdminCenter(QWidget *parent = nullptr);
    ~AdminCenter();

protected:
    void closeEvent(QCloseEvent *event) override;

private:
    Ui::AdminCenter *ui;
    QList<User> readUsersFromFile();
    void countStats(const QList<User>& users, int& numMen, int& numWomen, int ageGroups[5]);
    void createGenderChart(int numMen, int numWomen);
    void createAgeGroupChart(int ageGroups[5]);
    void populateUserTable(const QList<User>& users);
};

#endif // ADMINCENTER_H
