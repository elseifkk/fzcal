#include <stdio.h> // for debug

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
  initButIcon();
  shiftSlot();//<<< to set icons
  resultSet=false;

  this->setStyleSheet("QPushButton {background-color: rgb(30,30,30); border-style: outset; border-width: 2px; border-radius: 6px; border-color: rgb(50,50,50); color: white; font: bold} QPushButton:disabled {background-color: black; color: gray} QPushButton:checked {color: skyblue;} QPushButton:pressed {background-color: rgb(30,30,30); } QTextEdit {background-color: rgb(210,255,180); color: black; font: bold 14pt;}" );

}

void MainWindowImpl::mess(QString s, QColor c)
{
  resultBox->setTextColor(c);
  resultBox->setText(s);
  resultBox->setAlignment(Qt::AlignRight);
}

void MainWindowImpl::returnSlot(bool quiet)
{
  int rc;
  QString str;
  char cstr[LEN_FZCSTR_MAX];
  size_t pcstr= ( size_t ) &cstr[0];
  
  if ( formulaBox->lineEdit()->text().isEmpty() ) return;
  if(quiet) resultBox->clear();

  strcpy ( cstr,formulaBox->lineEdit()->text().toAscii() );
  rc=fzc_set_formula ( pfzc, pcstr );
  if ( rc>0 ){
    if(!quiet) mess ( "Syntacs Error","red" );
  }else if ( rc<0 ) {
    if(!quiet) mess ( "ok","blue" );
  }else {
    rc=fzc_eval ( pfzc );
    if ( rc!=0 ) {
      if(!quiet) mess ( "Eval Error","red" );
    }else{
      formulaBox->addToHistory ( formulaBox->lineEdit()->text() );
      fzc_get_strans ( pfzc, pcstr );
      str=cstr;
      mess ( str );
    }
  }
  resultSet=true;
}

void MainWindowImpl::textChangeSlot()
{
  if(autoBut->isChecked()) returnSlot(true);
}

void MainWindowImpl::setEngBut()
{
  if(shiftBut->isChecked()){
    dotBut->setText("ran");
    expBut->setText("pi");
    f0But->setText("");
    f1But->setText("f");
    f2But->setText("p");
    f3But->setText("n");
    f4But->setText("u");
    f5But->setText("m");
    f6But->setText("k");
    f7But->setText("M");
    f8But->setText("G");
    f9But->setText("T");
  }else{
    dotBut->setText(".");
    expBut->setText("Exp");
    f0But->setText("0");
    f1But->setText("1");
    f2But->setText("2");
    f3But->setText("3");
    f4But->setText("4");
    f5But->setText("5");
    f6But->setText("6");
    f7But->setText("7");
    f8But->setText("8");
    f9But->setText("9");
  }
}

void MainWindowImpl::dotSlot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText(".");
  }else{
    appendText("ran()");
  }
}
void MainWindowImpl::expSlot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("e");
  }else{
    appendText("pi");
  }
}
void MainWindowImpl::f0Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("0");
  }else{
  }
}
void MainWindowImpl::f1Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("1");
  }else{
    appendText("_f");
  }
}
void MainWindowImpl::f2Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("2");
  }else{
    appendText("_p");
  }
}
void MainWindowImpl::f3Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("3");
  }else{
    appendText("_n");
  }
}
void MainWindowImpl::f4Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("4");
  }else{
    appendText("_u");
  }
}
void MainWindowImpl::f5Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("5");
  }else{
    appendText("_m");
  }
}
void MainWindowImpl::f6Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("6");
  }else{
    appendText("_k");
  }
}
void MainWindowImpl::f7Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("7");
  }else{
    appendText("_M");
  }
}
void MainWindowImpl::f8Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("8");
  }else{
    appendText("_G");
  }
}
void MainWindowImpl::f9Slot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("9");
  }else{
    appendText("_T");
  }
}

void MainWindowImpl::ACSlot()
{
  formulaBox->lineEdit()->clear();
  resultBox->clear();
  resultSet=false;
}
void MainWindowImpl::delSlot()
{
  delText(1);
}
void MainWindowImpl::mulSlot()
{
  resultSet=false;
  appendText("*");
}
void MainWindowImpl::divSlot()
{
  resultSet=false;
  appendText("/");
}
void MainWindowImpl::addSlot()
{
  resultSet=false;
  appendText("+");
}
void MainWindowImpl::subSlot()
{
  resultSet=false;
  appendText("-");
}
void MainWindowImpl::ansSlot()
{
  tryClear();
  appendText("ans ");
}
void MainWindowImpl::EXESlot()
{
  returnSlot();
}

void MainWindowImpl::setButIcon(QPushButton *b, QString r)
{
  QIcon ic;
  ic.addPixmap(QPixmap(r));
  b->setIcon(ic);
  b->setIconSize(QSize(24,24));
}

void MainWindowImpl::initButIcon()
{
  setButIcon(aveBut,":/pix/xbar.png");
  setButIcon(varBut,":/pix/sigmax.png");
  setButIcon(uvarBut,":/pix/ux.png");
  setButIcon(sumBut,":/pix/sumx.png");
  setButIcon(sum2But,":/pix/sumx2.png");
  setButIcon(aveyBut,":/pix/ybar.png");
  setButIcon(varyBut,":/pix/sigmay.png");
  setButIcon(uvaryBut,":/pix/uy.png");
  setButIcon(sumyBut,":/pix/sumy.png");
  setButIcon(sumy2But,":/pix/sumy2.png");
  setButIcon(sumxyBut,":/pix/sumxy.png");
}

void MainWindowImpl::shiftSlot()
{
  QIcon ic;
  if(shiftBut->isChecked()){
    sqrtBut->setText("");
    setButIcon(sqrtBut,":/pix/x2.png");

    logBut->setText("");
    setButIcon(logBut,":/pix/10x.png");

    lnBut->setText("");
    setButIcon(lnBut,":/pix/exp.png");

    setButIcon(powBut,":/pix/xrt.png");

    minusBut->setText("");
    setButIcon(minusBut,":/pix/cbrt.png");
    
    stoBut->setText("Int");

    rclBut->setText("Frac");

    braBut->setText("");
    setButIcon(braBut,":/pix/xinv.png");

    ketBut->setText("");
    setButIcon(ketBut,":/pix/xfac.png");

    commaBut->setText(":");
    mpBut->setText("M-");

  }else{
    sqrtBut->setText("");
    setButIcon(sqrtBut,":/pix/sqrt.png");

    logBut->setIconSize(QSize(0,0));
    logBut->setText("log");

    lnBut->setIconSize(QSize(0,0));
    lnBut->setText("ln");

    powBut->setText("");
    setButIcon(powBut,":/pix/xy.png");

    minusBut->setIconSize(QSize(0,0));
    minusBut->setText("-");

    stoBut->setText("STO");

    rclBut->setText("RCL");

    braBut->setText("(");
    braBut->setIconSize(QSize(0,0));

    ketBut->setText(")");
    ketBut->setIconSize(QSize(0,0));

    commaBut->setText(",");
    mpBut->setText("M+");
  }
  setTriFncBut();
  setEngBut();
}
void MainWindowImpl::engSlot()
{
}
void MainWindowImpl::ratioSlot()
{
}
void MainWindowImpl::sqrtSlot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("sqrt(");
  }else{
    appendText("^2");
  }
}
void MainWindowImpl::logSlot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("log(");
  }else{
    appendText("10^");
  }
}
void MainWindowImpl::lnSlot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("ln(");
  }else{
    appendText("exp(");
  }
}
void MainWindowImpl::powSlot()
{
  tryClear();
  if(!shiftBut->isChecked()){
    appendText("^");
  }else{
    appendText("//");
  }
}
void MainWindowImpl::minusSlot()
{
  if(!shiftBut->isChecked()){
    appendText("-");
  }else{
    appendText("cbrt(");
  }
}
void MainWindowImpl::setTriFncBut()
{
  if(!hypBut->isChecked()){
    if(!shiftBut->isChecked()){
      sinBut->setText("sin");
      cosBut->setText("cos");
      tanBut->setText("tan");
    }else{
      sinBut->setText("asin");
      cosBut->setText("acos");
      tanBut->setText("atan");
    }
  }else{
    if(!shiftBut->isChecked()){
      sinBut->setText("sinh");
      cosBut->setText("cosh");
      tanBut->setText("tanh");
    }else{
      sinBut->setText("asinh");
      cosBut->setText("acosh");
      tanBut->setText("atanh");
    }
  }
}
void MainWindowImpl::hypSlot()
{
  setTriFncBut();
}
void MainWindowImpl::sinSlot()
{
  tryClear();
  appendText(sinBut->text()+"(");
}
void MainWindowImpl::cosSlot()
{
  tryClear();
  appendText(cosBut->text()+"(");
}
void MainWindowImpl::tanSlot()
{
  tryClear();
  appendText(tanBut->text()+"(");
}

void MainWindowImpl::delText(int n)
{
  if(formulaBox->lineEdit()->text().isEmpty()) return;
  int j=formulaBox->cursorPosition()-1;
  if(j<0) j=0;
  QString text=formulaBox->lineEdit()->text().remove(j,n);
  formulaBox->lineEdit()->setText(text);
}

void MainWindowImpl::appendText(QString s)
{
  int j=formulaBox->cursorPosition();
  QString text=formulaBox->lineEdit()->text().insert(j,s);
  formulaBox->lineEdit()->setText(text);
}

void MainWindowImpl::datSlot()
{
  if(datBut->isChecked()){
    staBut->setChecked(false);
    fzc_cle_opt(pfzc,FZCOPT_STA);
    fzc_set_opt(pfzc,FZCOPT_DAT);
  }else{
    fzc_cle_opt(pfzc,FZCOPT_DAT);
  }
}

void MainWindowImpl::staSlot()
{
  if(staBut->isChecked()){
    datBut->setChecked(false);
    fzc_cle_opt(pfzc,FZCOPT_DAT);
    fzc_set_opt(pfzc,FZCOPT_STA);
  }else{
    fzc_cle_opt(pfzc,FZCOPT_STA);
  }
  enableStaButs(staBut->isChecked());
}

void MainWindowImpl::enableStaButs(bool on)
{
  nBut->setEnabled(on);
  sumBut->setEnabled(on);
  sum2But->setEnabled(on);
  varBut->setEnabled(on);
  uvarBut->setEnabled(on);
  aveBut->setEnabled(on);
  sumyBut->setEnabled(on);
  sumy2But->setEnabled(on);
  varyBut->setEnabled(on);
  uvaryBut->setEnabled(on);
  aveyBut->setEnabled(on);
  sumxyBut->setEnabled(on);
}

void MainWindowImpl::nSlot()
{
  tryClear();
  appendText("n");
}
void MainWindowImpl::sumSlot()
{
  tryClear();
  appendText("sum");
}
void MainWindowImpl::sum2Slot()
{
  tryClear();
  appendText("sum2");
}
void MainWindowImpl::varSlot()
{
  tryClear();
  appendText("var");
}
void MainWindowImpl::uvarSlot()
{
  tryClear();
  appendText("uvar");
}
void MainWindowImpl::aveSlot()
{
  tryClear();
  appendText("ave");
}

void MainWindowImpl::cleSlot()
{
  fzc_cle_dat(pfzc);
}

void MainWindowImpl::autoSlot()
{}

void MainWindowImpl::tryClear()
{
  if(resultSet&&!autoBut->isChecked()){
    formulaBox->lineEdit()->clear();
    resultBox->clear();
    resultSet=false;
  }
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
