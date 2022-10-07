package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class OpenFileDialog extends AbstractDwcControl {

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS,
        OUTLINED_PRIMARY, OUTLINED_WARNING
    }
    
    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addFileChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            catchUp();
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }





    @Override
    public OpenFileDialog setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public OpenFileDialog setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public OpenFileDialog setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public OpenFileDialog setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public OpenFileDialog setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public OpenFileDialog setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public OpenFileDialog setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public OpenFileDialog addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public OpenFileDialog removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }




    public OpenFileDialog setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
