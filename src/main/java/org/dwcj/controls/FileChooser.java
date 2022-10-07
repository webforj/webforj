package org.dwcj.controls;

public class FileChooser extends AbstractDwcControl {

    @Override
    public FileChooser setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public FileChooser setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public FileChooser setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public FileChooser setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public FileChooser setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public FileChooser setID(String id){
        super.setControlID(id);
        return this;
    }

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

}
