package org.dwcj.controls.openfiledialog;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;

public final class OpenFileDialog extends AbstractDwcControl {

    public enum Theme{
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
        super.setText(text);
        return this;
    }

    @Override
    public OpenFileDialog setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public OpenFileDialog setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public OpenFileDialog setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public OpenFileDialog setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public OpenFileDialog setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public OpenFileDialog setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public OpenFileDialog addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public OpenFileDialog removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }




    public OpenFileDialog setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
