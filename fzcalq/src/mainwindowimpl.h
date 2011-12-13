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

 private:
  size_t pfzc;

  private slots:
  void returnSlot();
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

 protected:
  void keyPressEvent(QKeyEvent*);

};

#endif




