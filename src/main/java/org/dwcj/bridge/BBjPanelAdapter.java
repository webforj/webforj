package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.panels.AbstractDwcjPanel;

/**
 * ********** IMPORTANT: ****************+
 * This class is only needed for using DWCJ Controls from BBj code. It has no relevance to the Java development with DWCJ.
 * The BBjPanelAdapter converts a BBjWindow into an AbstractDwcPanel so that DWCJ Controls can
 * be added to code that is written in the BBj language.
 */
public class BBjPanelAdapter extends AbstractDwcjPanel {

    public BBjPanelAdapter(BBjWindow w){
        this.wnd = w;
    }

    @Override
    public AbstractDwcjPanel add(AbstractDwcControl ctrl) {
        try {
            ControlAccessor.getDefault().create(ctrl,this);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public BBjPanelAdapter setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public BBjPanelAdapter addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public BBjPanelAdapter removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }










    @Override
    public BBjPanelAdapter setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public BBjPanelAdapter setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public BBjPanelAdapter setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public BBjPanelAdapter setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public BBjPanelAdapter setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public BBjPanelAdapter setID(String id){
        super.setControlID(id);
        return this;
    }



}
