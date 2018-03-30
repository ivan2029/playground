#include "pop_up.hpp"

#include <QApplication>
#include <QDesktopWidget>
#include <QMouseEvent>
#include <QDateTime>

#include <QDebug>

Popup::Popup(QWidget *parent)
    : QWidget(parent)
{
    this->setWindowFlags( Qt::Window
                        | Qt::FramelessWindowHint
                        | Qt::WindowDoesNotAcceptFocus
                        | Qt::WindowStaysOnTopHint
                        );
    this->setAttribute( Qt::WA_ShowWithoutActivating );

    this->setStyleSheet(R"(
        QWidget {
            color: #ff8080;
            background-color: #2e2f30;
        })");

    //
    m_label.setText(tr("Mrdaj dupe!"));

    m_layout.addWidget(&m_label,0,0, Qt::AlignCenter);

    this->setLayout(&m_layout);

    //
    connect( &m_timer, &QTimer::timeout
           , this,     &Popup::on_timeout );

    //
    m_timer.setSingleShot(true);
}

auto Popup::show_popup( int screen
                      , int screen_portion
                      , int popup_remains_for_ms
                      , QString message
) -> void {
    auto const ratio = screen_portion/100.0f;

    auto const screen_id   = screen != -1
                           ? screen
                           : QApplication::desktop()->primaryScreen();
    auto const screen_rect = QApplication::desktop()->screen(screen_id)->geometry();

    auto const width  = static_cast<int>(screen_rect.width()*ratio);
    auto const height = static_cast<int>(screen_rect.height()*ratio);
    auto const x = screen_rect.x() + (screen_rect.width() - width)/2;
    auto const y = screen_rect.y() + (screen_rect.height() - height)/2;

    this->resize(width, height);
    this->move(x, y);

    auto font = m_label.font();
    font.setPixelSize(width/10);
    m_label.setFont(font);

    m_label.setText(message);

    this->start_timer(popup_remains_for_ms);

    this->show();
}

auto Popup::start_timer(int popup_remains_for_ms) -> void {
    m_remain_for = popup_remains_for_ms;
    m_started_at = QDateTime::currentMSecsSinceEpoch();
    m_timer.start(1000);
}

auto Popup::on_timeout() -> void {
    auto const now = QDateTime::currentMSecsSinceEpoch();

//    qDebug() << "popup: tryin at " << now << ", diff " << now - m_started_at;

    if(now - m_started_at > m_remain_for) {
        this->hide();
        emit popup_closed();
    } else {
        m_timer.start(1000);
    }
}

auto Popup::mousePressEvent(QMouseEvent *event) -> void {
}

auto Popup::mouseReleaseEvent(QMouseEvent *event) -> void {
    if(!event->button() == Qt::LeftButton) return;

    auto const mouse_pos = event->screenPos().toPoint();
    auto const popup_rect = this->geometry();

    if(popup_rect.contains(mouse_pos)) {
        this->hide();
        this->m_timer.stop();
        emit popup_closed();
    }
}
