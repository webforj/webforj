package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.bbj.proxies.sysgui.BBjImageCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.awt.*;

public final class ImageControl extends AbstractDwcControl implements IStyleable {

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
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
