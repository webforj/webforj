package org.dwcj.controls;


public interface IThemable {

    void setTheme(Theme theme);

    enum Theme {
        DEFAULT,
        DANGER,
        GRAY,
        INFO,
        PRIMARY,
        SUCCESS,
        WARNING
    }

}
