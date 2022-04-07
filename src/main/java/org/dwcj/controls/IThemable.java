package org.dwcj.controls;

public interface IThemable {

    /**
     * set the theme to the control
     *
     * @param theme the theme to apply
     * @return the control itself
     */
    IThemable setTheme(Theme theme);

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
