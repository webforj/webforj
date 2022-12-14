package org.dwcj.controls.imagecontrol;

import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.bbj.proxies.sysgui.BBjImageCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;

import java.awt.*;

public final class ImageControl extends AbstractDwcControl {

    private BBjImageCtrl bbjImageControl;

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = w.addImageCtrl(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            bbjImageControl = (BBjImageCtrl) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Image getImage() {
        try {
            return (Image) bbjImageControl.getImage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean isDisableable() {
        return bbjImageControl.isDisableable();
    }

    public void setDisableable(boolean disableable) {
        bbjImageControl.setDisableable(disableable);
    }

    public void setImage(Image image) {
        try {
            bbjImageControl.setImage((BBjImage) image);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public ImageControl setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public ImageControl setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public ImageControl setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public ImageControl setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public ImageControl setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public ImageControl setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public ImageControl setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public ImageControl addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public ImageControl removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }
}
