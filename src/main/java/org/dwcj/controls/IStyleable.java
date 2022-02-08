package org.dwcj.controls;

public interface IStyleable {

    void setStyle(String property, String value);

    void addClass(String selector);

    void removeClass(String selector);

}
