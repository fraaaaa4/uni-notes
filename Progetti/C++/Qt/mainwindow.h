#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "admincenter.h"
#include "subscribe.h"
#include "forgotcredentials.h"


QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private:
    Ui::MainWindow *ui;
    bool verifyAdminLogin(QString email, QString password);
    bool verifyLogin(QString email, QString password);

private slots:
    void openAdminCenter();
    void subscribeClicked();
    void onLoginClicked();
    void onForgotClicked();

private:
    AdminCenter *adminCenter;
    subscribe *subscribeForm;
    forgotCredentials *forgot_credentials;
};
#endif // MAINWINDOW_H
