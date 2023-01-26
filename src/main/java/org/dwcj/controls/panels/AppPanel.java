package org.dwcj.controls.panels;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcAppInitializeException;

/**
 * AppPanel is the core main application panel.
 * It typically occupied the full browser real estate and holds your app
 * that consists of div container panels
 */
public class AppPanel extends AbstractDwcjPanel {

    public AppPanel() throws DwcAppInitializeException {

        try {
            byte[] flags = new byte[]{(byte) 0x01, (byte) 0x11, (byte) 0x10, (byte)0x88};
            BasisNumber b1 = BasisNumber.createBasisNumber(1);
            BasisNumber ctx = BasisNumber.createBasisNumber(Environment.getInstance().getSysGui().getAvailableContext());
            wnd = Environment.getInstance().getSysGui().addWindow(ctx, b1, b1, b1, b1, "AppPanel", flags);
            ctrl = wnd;
        } catch (NumberFormatException | BBjException e) {
            Environment.logError(e);
            throw new DwcAppInitializeException(e);
        }

    }


    /**
     * create a Byte Array out of an Integer
     * @param val the integer value to convert
     * @return a byte array representing that integer
     */
    private static byte[] getByteArray(int val) {
        return new byte[] {
                (byte)(val>>24), (byte)(val>>16), (byte)(val>>8), (byte)val
        };
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        //empty, needs override
    }

    @Override
    public AppPanel setStyle(String property, String value) {
        wnd.setPanelStyle(property, value);
        return this;
    }



    @Override
    public AppPanel setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public AppPanel setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public AppPanel setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public AppPanel setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public AppPanel setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public AppPanel setId(String elementId){
        super.setId(elementId);
        return this;
    }
    
    @Override
    public AppPanel addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public AppPanel removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }
}
