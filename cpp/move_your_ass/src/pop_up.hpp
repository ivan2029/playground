#ifndef POP_UP_HPP
#define POP_UP_HPP

#include <QWidget>
#include <QGridLayout>
#include <QLabel>
#include <QTimer>

class Popup : public QWidget {
    Q_OBJECT
public:
    explicit Popup(QWidget *parent = nullptr);

signals:
    void popup_closed();

public slots:
    void show_popup(int screen, int screen_portion, int popup_remains_for_ms);

private:
    //
    auto start_timer(int popup_remains_for) -> void;
    auto on_timeout() -> void;

    //
    auto mousePressEvent(QMouseEvent* event) -> void override;
    auto mouseReleaseEvent(QMouseEvent* event) -> void override;

private:
    QGridLayout  m_layout;
    QLabel       m_label;

    QTimer m_timer;
    int    m_remain_for{0}; // milliseconds
    qint64 m_started_at{0};
};

#endif // POP_UP_HPP
