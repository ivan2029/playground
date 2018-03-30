#include "main_window.hpp"

#include <QApplication>
#include <QDesktopWidget>
#include <QDateTime>

#include <QDebug>

//
//
//
auto mins_to_ms(int minutes) -> int {
    return minutes*60*1000;
}

//
//
//
MainWindow::MainWindow(QWidget *parent)
    : QWidget(parent)
{
    this->setWindowTitle(tr("Move your ass!"));

    this->setup_screen_portion();
    this->setup_till_next();
    this->setup_popup_remains_for();
    this->setup_message();
    this->setup_which_screen();
    this->setup_activity_label();
    this->setup_start_button();
    this->add_widgets_to_layout();
    this->setup_connections();

    m_timer.setSingleShot(true);
}

auto MainWindow::popup_closed() -> void {
    m_activity_label.hide();
    m_start_button.setText(tr("Start"));
    this->set_all_enabled(true);
}

auto MainWindow::setup_screen_portion() -> void {
    m_screen_portion_validator.setRange(10, 100);
    m_screen_portion_prefix.setText(tr("Screen portion: "));
    m_screen_portion_input.setText(QString{"%1"}.arg(m_timer_settings.screen_portion));
    m_screen_portion_input.setMaximumWidth(50);
    m_screen_portion_input.setValidator(&m_screen_portion_validator);
    m_screen_portion_postfix.setText(tr("%"));
}

auto MainWindow::setup_till_next() -> void {
    m_till_next_validator.setRange(1, 60);
    m_till_next_prefix.setText(tr("Time till next popup"));
    m_till_next_input.setText(QString{"%1"}.arg(m_timer_settings.till_next_time));
    m_till_next_input.setMaximumWidth(50);
    m_till_next_input.setValidator(&m_till_next_validator);
    m_till_next_postfix.setText(tr("minutes"));
}

auto MainWindow::setup_popup_remains_for() -> void {
    m_popup_remains_for_validator.setRange(1, 60);
    m_popup_remains_for_prefix.setText(tr("Popup remains for"));
    m_popup_remains_for_input.setText(QString{"%1"}.arg(m_timer_settings.popup_remains_for_time));
    m_popup_remains_for_input.setMaximumWidth(50);
    m_popup_remains_for_input.setValidator(&m_popup_remains_for_validator);
    m_popup_remains_for_postfix.setText(tr("minutes"));
}

auto MainWindow::setup_message() -> void {
    m_message_label.setText(tr("Message"));
    m_message_input.setText(tr("Move your lazy ass!"));
    m_message_input.setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
}

auto MainWindow::setup_which_screen() -> void {
    m_timer_settings.which_screen = QApplication::desktop()->primaryScreen();
    auto const screen_count = QApplication::desktop()->screenCount();
    if(screen_count > 1) {
        m_which_screen_label.setText(tr("Which screen: "));
        m_which_screen_test.setText(tr("Show popup on this screen"));
        for(int i = 0; i != screen_count; ++ i) {
            m_which_screen_combo.addItem(QString{"%1"}.arg(i));
        }
    }
    else {
        m_which_screen_label.hide();
        m_which_screen_combo.hide();
    }
}

auto MainWindow::setup_activity_label() -> void {
    m_activity_label.hide();
}

auto MainWindow::setup_start_button() -> void {
    m_start_button.setText(tr("Start"));
}

auto MainWindow::add_widgets_to_layout() -> void {
    m_layout.addWidget(  &m_screen_portion_prefix, 0, 0,   Qt::AlignLeft);
    m_layout.addWidget(   &m_screen_portion_input, 0, 1, Qt::AlignCenter);
    m_layout.addWidget( &m_screen_portion_postfix, 0, 2,   Qt::AlignLeft);

    m_layout.addWidget(  &m_till_next_prefix, 1, 0,   Qt::AlignLeft);
    m_layout.addWidget(   &m_till_next_input, 1, 1, Qt::AlignCenter);
    m_layout.addWidget( &m_till_next_postfix, 1, 2,   Qt::AlignLeft);

    m_layout.addWidget(  &m_popup_remains_for_prefix, 2, 0,   Qt::AlignLeft);
    m_layout.addWidget(   &m_popup_remains_for_input, 2, 1, Qt::AlignCenter);
    m_layout.addWidget( &m_popup_remains_for_postfix, 2, 2,   Qt::AlignLeft);

    m_layout.addWidget(&m_message_label, 3, 0, Qt::AlignLeft);
    m_layout.addWidget(&m_message_input, 3, 1, 1, 2);

    m_layout.addWidget(&m_which_screen_label, 4, 0, Qt::AlignLeft);
    m_layout.addWidget(&m_which_screen_combo, 4, 1, Qt::AlignCenter);
    m_layout.addWidget( &m_which_screen_test, 4, 2, Qt::AlignCenter);

    m_layout.addWidget(&m_activity_label, 5, 0, 1, 2, Qt::AlignCenter);

    m_layout.addWidget(&m_start_button, 5, 2, Qt::AlignCenter);

    this->setLayout(&m_layout);
}

auto MainWindow::setup_connections() -> void {
    connect( &m_start_button, &QPushButton::clicked
           , this,            &MainWindow::start_countdown );
    connect( &m_timer, &QTimer::timeout
           , this,     &MainWindow::on_timeout );
    connect( &m_which_screen_test, &QPushButton::clicked
           , this                , &MainWindow::on_which_screen_test_clicked );
}

auto MainWindow::set_all_enabled(bool enabled) -> void {
    m_screen_portion_input.setEnabled(enabled);
    m_till_next_input.setEnabled(enabled);
    m_popup_remains_for_input.setEnabled(enabled);
    m_start_button.setEnabled(enabled);
}

auto MainWindow::start_countdown(bool) -> void
{
    m_timer_settings.screen_portion         = m_screen_portion_input.text().toInt();
    m_timer_settings.till_next_time         = m_till_next_input.text().toInt();
    m_timer_settings.popup_remains_for_time = m_popup_remains_for_input.text().toInt();
    m_timer_settings.message                = m_message_input.text();

    if(QApplication::desktop()->screenCount() > 1) {
        m_timer_settings.which_screen = m_which_screen_combo.currentText().toInt();
    }

    m_timer_settings.start_time = QDateTime::currentMSecsSinceEpoch();

    m_activity_label.setText(tr("Counting down!"));
    m_activity_label.show();
    m_start_button.setText(tr("Reset"));

    m_timer.start(1000);
}

auto MainWindow::on_timeout() -> void {
    auto now = QDateTime::currentMSecsSinceEpoch();

//    qDebug() << "window: tryin at " << now << ", diff " << now - m_timer_settings.start_time;

    if(now - m_timer_settings.start_time > mins_to_ms(m_timer_settings.till_next_time)) {
        m_activity_label.setText(tr("Showing popup"));
        set_all_enabled(false);
        emit show_popup( m_timer_settings.which_screen
                       , m_timer_settings.screen_portion
                       , mins_to_ms(m_timer_settings.popup_remains_for_time)
                       , m_timer_settings.message );
    }
    else { // try later
        m_timer.start(1000);
    }
}

auto MainWindow::on_which_screen_test_clicked() -> void {
    auto const screen_portion = m_screen_portion_input.text().toInt();
    auto const screen_id = m_which_screen_combo.currentText().toInt();
    auto const message = m_message_input.text();

    m_activity_label.setText(tr("Showing screen"));
    set_all_enabled(false);
    emit show_popup( screen_id
                   , screen_portion
                   , 5000
                   , std::move(message) );
}

auto MainWindow::closeEvent(QCloseEvent *) -> void {
    emit widget_closed();
}
