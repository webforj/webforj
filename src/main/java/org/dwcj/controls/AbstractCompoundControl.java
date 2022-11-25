package org.dwcj.controls;

import org.dwcj.panels.AbstractDwcjPanel;
import org.dwcj.panels.Div;



public abstract class AbstractCompoundControl extends AbstractDwcControl{

    protected Div canvas = new Div();

    @Override
    public void create(AbstractDwcjPanel panel) {
        panel.add(canvas);
        init();
    }

    public void addComponent(AbstractDwcControl control){
        canvas.add(control);
    }

    public abstract void init();





    public AbstractCompoundControl setText(String text){
        super.setControlText(text);
        return this;
    }

    public AbstractCompoundControl setVisible(Boolean visible){
        canvas.setControlVisible(visible);
        return this;
    }
    
    public AbstractCompoundControl setEnabled(Boolean enabled) {
        canvas.setControlEnabled(enabled);
        return this;
    }

    public AbstractCompoundControl setTooltipText(String text) {
        canvas.setControlTooltipText(text);
        return this;
    }

    public AbstractCompoundControl setAttribute(String attribute, String value){
        canvas.setControlAttribute(attribute, value);
        return this;
    }

    public AbstractCompoundControl setID(String id){
        canvas.setControlID(id);
        return this;
    }

    public AbstractCompoundControl setStyle(String property, String value) {
        canvas.setControlStyle(property, value);
        return this;
    }
    
    public AbstractCompoundControl addClass(String selector) {
        canvas.addControlCssClass(selector);
        return this;
    }

    public AbstractCompoundControl removeClass(String selector) {
        canvas.removeControlCssClass(selector);
        return this;
    }
    
}
