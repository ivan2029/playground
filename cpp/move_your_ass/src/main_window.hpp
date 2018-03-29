#ifndef MAIN_WINDOW_HPP
#define MAIN_WINDOW_HPP

#include <QWidget>

#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QComboBox>
#include <QIntValidator>

#include <QTimer>

struct TimerSettings {
    int  screen_portion{50}; // percentage
    int  till_next_time{30}; // minutes
    int  popup_remains_for_time{5}; // minutes
    int  which_screen{-1};
    qint64 start_time{0};
};

class MainWindow : public QWidget {
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);

signals:
    void show_popup(int screen, int screen_portion, int popup_remains_for_ms);
    void widget_closed();

public slots:
    void popup_closed();

private:

    // called during construction
    auto setup_screen_portion() -> void;
    auto setup_till_next() -> void;
    auto setup_popup_remains_for() -> void;
    auto setup_which_screen() -> void;
    auto setup_activity_label() -> void;
    auto setup_start_button() -> void;
    auto add_widgets_to_layout() -> void;
    auto setup_connections() -> void;

    //
    auto set_all_enabled(bool enabled) -> void;

    //
    auto start_countdown(bool /*unused*/) -> void;

    auto on_timeout() -> void;

    auto on_which_screen_test_clicked() -> void;

    //
    auto closeEvent(QCloseEvent*) -> void override;

private:
    //
    QGridLayout m_layout;

    QLabel        m_screen_portion_prefix;
    QLineEdit     m_screen_portion_input;
    QIntValidator m_screen_portion_validator;
    QLabel        m_screen_portion_postfix;

    QLabel        m_till_next_prefix;
    QLineEdit     m_till_next_input;
    QIntValidator m_till_next_validator;
    QLabel        m_till_next_postfix;

    QLabel        m_popup_remains_for_prefix;
    QLineEdit     m_popup_remains_for_input;
    QIntValidator m_popup_remains_for_validator;
    QLabel        m_popup_remains_for_postfix;

    QLabel        m_which_screen_label;
    QComboBox     m_which_screen_combo;
    QPushButton   m_which_screen_test;

    QLabel        m_activity_label;

    QPushButton   m_start_button;

    //
    QTimer m_timer;
    QMetaObject::Connection m_timer_connection;

    // Q: any benefit from these being Q_PROPERTIY
    TimerSettings m_timer_settings;
};

#endif // MAIN_WINDOW_HPP
