package org.dwcj.controls.filechooser;

import org.dwcj.controls.AbstractDwcControl;

public class FileChooser extends AbstractDwcControl {

    @Override
    public FileChooser setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public FileChooser setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public FileChooser setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public FileChooser setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public FileChooser setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public FileChooser setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public FileChooser setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public FileChooser addClass(String selector) {
        super.addClass(selector);
        return this;
    }

    @Override
    public FileChooser removeClass(String selector) {
        super.removeClass(selector);
        return this;
    }

}
