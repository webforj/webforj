package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;

import org.dwcj.Environment;
import org.dwcj.annotation.AnnotationProcessor;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.panels.AbstractPanel;

/**
 * ********** IMPORTANT: ****************+
 * This class is only needed for using DWCJ Controls from BBj code. It has no relevance to the Java development with DWCJ.
 * The BBjPanelAdapter converts a BBjWindow into an AbstractDwcPanel so that DWCJ Controls can
 * be added to code that is written in the BBj language.
 */
public class BBjPanelAdapter extends AbstractPanel {

    public BBjPanelAdapter(BBjWindow w){
        this.wnd = w;
    }

    /**
     * Used to add controls to a panel. Multiple controls can be passed to this
     * function, and will be added in the order the arguments are passed 
     * (arg0 added first, arg1 second, etc...)
     * @param ctrl the control(s) to be added
     * @return the panel itself
     */
    @Override
    public AbstractPanel add(AbstractComponent ...ctrl) {
        for(AbstractComponent c: ctrl){
            try {
                AnnotationProcessor processor = new AnnotationProcessor();
                processor.processControlAnnotations(c);
                ComponentAccessor.getDefault().create(c,this);
            } catch (IllegalAccessException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    @Override
    public BBjPanelAdapter setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }

    @Override
    public BBjPanelAdapter addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public BBjPanelAdapter removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


    @Override
    public BBjPanelAdapter setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public BBjPanelAdapter setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public BBjPanelAdapter setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public BBjPanelAdapter setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public BBjPanelAdapter setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public BBjPanelAdapter setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    protected void create(AbstractPanel p) {
        // nothing to do here
    }
}
