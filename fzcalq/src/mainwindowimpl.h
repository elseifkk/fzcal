#ifndef MAINWINDOWIMPL_H
#define MAINWINDOWIMPL_H

//
#include <QMainWindow>
#include "ui_mainwindow.h"

//
class MainWindowImpl : public QMainWindow, public Ui::MainWindow
{
  Q_OBJECT
    
    public:
  MainWindowImpl( QWidget * parent = 0, Qt::WFlags f = 0 );
  
 private:
  void appendText(QString);
  void mess(QString,QColor="black");
  void delText(int i=1);
  void setTriFncBut();
  void setButIcon(QPushButton *, QString r);
  void enableStaButs(bool on);
  void tryClear();
  void setEngBut();
  void initButIcon();

 private:
  size_t pfzc;
  bool resultSet;

  private slots:
  void returnSlot(bool quiet=false);
  void f0Slot();
  void f1Slot();
  void f2Slot();
  void f3Slot();
  void f4Slot();
  void f5Slot();
  void f6Slot();
  void f7Slot();
  void f8Slot();
  void f9Slot();
  void dotSlot();
  void expSlot();
  void ACSlot();
  void delSlot();
  void mulSlot();
  void divSlot();
  void subSlot();
  void addSlot();
  void ansSlot();
  void EXESlot();
  void shiftSlot();
  void engSlot();
  void ratioSlot();
  void sqrtSlot();
  void logSlot();
  void lnSlot();
  void powSlot();
  void minusSlot();
  void hypSlot();
  void sinSlot();
  void cosSlot();
  void tanSlot();
  void textChangeSlot();
  void datSlot();
  void staSlot();
  void nSlot();
  void sumSlot();
  void sum2Slot();
  void aveSlot();
  void varSlot();
  void uvarSlot();
  void autoSlot();
  void cleSlot();

 protected:
  void keyPressEvent(QKeyEvent*);

};

#endif




