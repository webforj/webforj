package org.dwcj.controls;

public class FileChooser extends AbstractDwcControl {

    public FileChooser setText(String text) {
        super.setControlText(text);
        return this;
    }

    public FileChooser setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public FileChooser setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public FileChooser setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public FileChooser setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public FileChooser setID(String id){
        super.setControlID(id);
        return this;
    }

    public FileChooser setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public FileChooser addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public FileChooser removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

}
