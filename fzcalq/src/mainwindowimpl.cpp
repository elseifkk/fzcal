#include "mainwindowimpl.h"
#include <fzc.h>
//

#include <QLineEdit>
#include <QTextEdit>
#include <QString>
#include <QKeyEvent>

MainWindowImpl::MainWindowImpl( QWidget * parent, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  setupUi(this);
  pfzc=fzc_init();
}


void MainWindowImpl::mess(QString s, QColor c)
{
  resultBox->setTextColor(c);
  resultBox->setText(s);
}

void MainWindowImpl::returnSlot()
{
  int rc;
  QString str;
  char cstr[LEN_FZCSTR_MAX];
  size_t pcstr= ( size_t ) &cstr[0];
  
  if ( formulaBox->lineEdit()->text().isEmpty() ) return;

  strcpy ( cstr,formulaBox->lineEdit()->text().toAscii() );
  rc=fzc_set_formula ( pfzc, pcstr );
  if ( rc>0 ){
    mess ( "Syntacs Error","red" );
  }else if ( rc<0 ) {
    mess ( "ok","blue" );
  }else {
    rc=fzc_eval ( pfzc );
    if ( rc!=0 ) {
      mess ( "Eval Error","red" );
      return;
    }
    formulaBox->addToHistory ( formulaBox->lineEdit()->text() );
    fzc_get_strans ( pfzc, pcstr );
    str=cstr;
    mess ( str );
  }
}

void MainWindowImpl::dotSlot()
{
  appendText(".");
}
void MainWindowImpl::expSlot()
{
  appendText("e");
}
void MainWindowImpl::f0Slot()
{
  appendText("0");
}
void MainWindowImpl::f1Slot()
{
  appendText("1");
}
void MainWindowImpl::f2Slot()
{
  appendText("2");
}
void MainWindowImpl::f3Slot()
{
  appendText("3");
}
void MainWindowImpl::f4Slot()
{
  appendText("4");
}
void MainWindowImpl::f5Slot()
{
  appendText("5");
}
void MainWindowImpl::f6Slot()
{
  appendText("6");
}
void MainWindowImpl::f7Slot()
{
  appendText("7");
}
void MainWindowImpl::f8Slot()
{
  appendText("8");
}
void MainWindowImpl::f9Slot()
{
  appendText("9");
}

void MainWindowImpl::ACSlot()
{
  formulaBox->lineEdit()->clear();
}
void MainWindowImpl::delSlot()
{
  delText(1);
}
void MainWindowImpl::mulSlot()
{
  appendText("*");
}
void MainWindowImpl::divSlot()
{
  appendText("/");
}
void MainWindowImpl::addSlot()
{
  appendText("+");
}
void MainWindowImpl::subSlot()
{
  appendText("-");
}
void MainWindowImpl::ansSlot()
{
  appendText("ans ");
}
void MainWindowImpl::EXESlot()
{
  returnSlot();
}

void MainWindowImpl::delText(int n)
{
  if(formulaBox->lineEdit()->text().isEmpty()) return;
  int j=formulaBox->cursorPosition();
  if(j>=formulaBox->lineEdit()->text().length()) j--;
  QString text=formulaBox->lineEdit()->text().remove(j,n);
  formulaBox->lineEdit()->setText(text);
}

void MainWindowImpl::appendText(QString s)
{
  int j=formulaBox->cursorPosition();
  QString text=formulaBox->lineEdit()->text().insert(j,s);
  formulaBox->lineEdit()->setText(text);
}

void MainWindowImpl::keyPressEvent(QKeyEvent *e)
{
  switch(e->key())
    {
    case Qt::Key_0:
      f0Slot();
      break;
    case Qt::Key_1:
      f1Slot();
      break;
    case Qt::Key_2:
      f2Slot();
      break;
    case Qt::Key_3:
      f3Slot();
      break;
    case Qt::Key_4:
      f4Slot();
      break;
    case Qt::Key_5:
      f5Slot();
      break;
    case Qt::Key_6:
      f6Slot();
      break;
    case Qt::Key_7:
      f7Slot();
      break;
    case Qt::Key_8:
      f8Slot();
      break;
    case Qt::Key_9:
      f9Slot();
      break;
    case Qt::Key_Plus:
      addSlot();
      break;
    case Qt::Key_Minus:
      subSlot();
      break;
    case Qt::Key_Delete:
      delSlot();
      break;
    case Qt::Key_Escape:
      formulaBox->clearFocus();
      break;
    case Qt::Key_Asterisk:
      mulSlot();
      break;
    case Qt::Key_Slash:
      divSlot();
    case Qt::Key_Period:
      dotSlot();
      break;
    case Qt::Key_Return:
      returnSlot();
      break;
    case Qt::Key_E:
      expSlot();
      break;
    default:
      e->ignore();
      return;
    }
  e->accept();
}
//
