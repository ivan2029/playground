#include <QApplication>
#include <QDebug>
#include "main_window.hpp"
#include "pop_up.hpp"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    MainWindow window;
    Popup      popup;

    QObject::connect( &window, &MainWindow::show_popup
                    , &popup, &Popup::show_popup );
    QObject::connect( &window, &MainWindow::widget_closed
                    , &popup, &Popup::close );

    QObject::connect( &popup, &Popup::popup_closed
                    , &window, &MainWindow::popup_closed );

    popup.hide();
    window.show();
    return a.exec();
}


/*
 *
#include <QDesktopWidget>
#include <QWidget>
#include <QTimer>


    QRect rect = QApplication::desktop()->geometry();

    QWidget w;

    w.setWindowFlags(Qt::Window | Qt::FramelessWindowHint | Qt::WindowDoesNotAcceptFocus | Qt::WindowStaysOnTopHint);
    w.setAttribute(Qt::WA_ShowWithoutActivating);

    w.resize(QSize{400, 400});
    w.move(rect.width()-400, rect.height()-450);

    w.show();

    QTimer timer;

    QObject::connect(&timer, &QTimer::timeout, [&]{
        w.close();
    });

    timer.start(5000);
*/
