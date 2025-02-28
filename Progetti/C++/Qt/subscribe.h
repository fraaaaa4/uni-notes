#ifndef SUBSCRIBE_H
#define SUBSCRIBE_H

#include <QDialog>

namespace Ui {
class subscribe;
}

class subscribe : public QDialog
{
    Q_OBJECT

public:
    explicit subscribe(QWidget *parent = nullptr);
    ~subscribe();

protected:
    void closeEvent(QCloseEvent *event) override;

private:
    Ui::subscribe *ui;
    bool checkNotAdmin(QString email);

private slots:
    void saveUserToFile();
};

#endif // SUBSCRIBE_H
