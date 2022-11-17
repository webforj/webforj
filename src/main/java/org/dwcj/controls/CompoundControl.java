package org.dwcj.controls;

import org.dwcj.panels.AbstractDwcjPanel;
import org.dwcj.panels.Div;



public abstract class CompoundControl extends AbstractDwcControl{

    protected Div wrapper = new Div();

    @Override
    public void create(AbstractDwcjPanel panel) {
        panel.add(wrapper);
        build();
    }

    public void addComponent(AbstractDwcControl control){
        wrapper.add(control);
    }

    public abstract void build();






    public CompoundControl setText(String text){
        wrapper.setControlText(text);
        return this;
    }

    public CompoundControl setVisible(Boolean visible){
        wrapper.setControlVisible(visible);
        return this;
    }
    
    public CompoundControl setEnabled(Boolean enabled) {
        wrapper.setControlEnabled(enabled);
        return this;
    }

    public CompoundControl setTooltipText(String text) {
        wrapper.setControlTooltipText(text);
        return this;
    }

    public CompoundControl setAttribute(String attribute, String value){
        wrapper.setControlAttribute(attribute, value);
        return this;
    }

    public CompoundControl setID(String id){
        wrapper.setControlID(id);
        return this;
    }

    public CompoundControl setStyle(String property, String value) {
        wrapper.setControlStyle(property, value);
        return this;
    }
    
    public CompoundControl addClass(String selector) {
        wrapper.addControlCssClass(selector);
        return this;
    }

    public CompoundControl removeClass(String selector) {
        wrapper.removeControlCssClass(selector);
        return this;
    }
    
}
