#ifndef FORGOTCREDENTIALS_H
#define FORGOTCREDENTIALS_H

#include <QDialog>

namespace Ui {
class forgotCredentials;
}

class forgotCredentials : public QDialog
{
    Q_OBJECT

public:
    explicit forgotCredentials(QWidget *parent = nullptr);
    ~forgotCredentials();

protected:
    void closeEvent(QCloseEvent *event) override;

private:
    Ui::forgotCredentials *ui;

private slots:
    void recoverPassword();
};

#endif // FORGOTCREDENTIALS_H
