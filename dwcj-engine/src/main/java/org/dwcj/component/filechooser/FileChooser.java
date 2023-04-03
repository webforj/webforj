package org.dwcj.component.filechooser;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractPanel;

public final class FileChooser extends AbstractDwcComponent {

    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS,
        OUTLINED_PRIMARY, OUTLINED_WARNING
    }
    
    @Override
    protected void create(AbstractPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addFileChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            catchUp();
        } catch (Exception e)  {
            Environment.logError(e);
        }
    }





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
    public FileChooser setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public FileChooser setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public FileChooser addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public FileChooser removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }




    public FileChooser setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
