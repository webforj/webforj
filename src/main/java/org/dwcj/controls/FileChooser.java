package org.dwcj.controls;

public class FileChooser extends AbstractDwcControl {

    @Override
    public FileChooser setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public FileChooser addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public FileChooser removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public FileChooser setID(String id){
        super.setID(id);
        return this;
    }

}
