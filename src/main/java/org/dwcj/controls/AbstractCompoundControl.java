package org.dwcj.controls;

import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.panels.Div;



public abstract class AbstractCompoundControl extends AbstractDwcControl{

    protected Div canvas = new Div();

    /*
     * Will need to have a create() method for any controls that extend this class, but it's
     * acceptable to simply call the super.create() method and pass the AbstractDwcjPanel from
     * the custom compound control's create function into that of the super.create().
     */
    @Override
    public void create(AbstractDwcjPanel panel) {
        panel.add(canvas);
        init();
    }

    public void addComponent(AbstractDwcControl control){
        canvas.add(control);
    }

    /*
     * Overwrite the init() function with the desired
     * implementation of your compound control. This is where adding
     * various components, styling them, etc can take place.
     */
    public abstract void init();




    /*
     * Developers may want to override some of these methods for 
     * their specific endgoals, such as having setText work in a 
     * more specific way for their custom group of controls.
     */

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
