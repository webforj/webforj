package org.dwcj.controls.fontChooser;

import com.basis.bbj.proxies.sysgui.BBjFont;
import com.basis.bbj.proxies.sysgui.BBjFontChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.fontChooser.events.FontChooserApproveEvent;
import org.dwcj.controls.fontChooser.events.FontChooserCancelEvent;
import org.dwcj.controls.fontChooser.events.FontChooserChangeEvent;
import org.dwcj.controls.fontChooser.sinks.FontChooserApproveEventSink;
import org.dwcj.controls.fontChooser.sinks.FontChooserCancelEventSink;
import org.dwcj.controls.fontChooser.sinks.FontChooserChangeEventSink;
import org.dwcj.controls.panels.AbstractDwcjPanel;

import java.awt.*;
import java.util.function.Consumer;


public final class FontChooser extends AbstractDwcControl {

    private FontChooserApproveEventSink fontChooserApproveEventSink;

    private FontChooserCancelEventSink fontChooserCancelEventSink;

    private FontChooserChangeEventSink fontChooserChangeEventSink;

    private BBjFontChooser bbjFontChooser;

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visbility flag
            ctrl = w.addFontChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            bbjFontChooser = (BBjFontChooser) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public FontChooser approveSelection() {
        try {
            bbjFontChooser.approveSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser cancelSelection() {
        try {
            bbjFontChooser.cancelSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public String getApproveButtonText() {
        try {
            return bbjFontChooser.getApproveButtonText();
        } catch (BBjException e) {
            e.printStackTrace();
            return "";
        }
    }

    public String getCancelButtonText() {
        try {
            return bbjFontChooser.getCancelButtonText();
        } catch (BBjException e) {
            e.printStackTrace();
            return "";
        }
    }

    public boolean isControlButtonsAreShown() {
        try {
            return bbjFontChooser.getControlButtonsAreShown();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean isFontsScaled() {
        try {
            return bbjFontChooser.getFontsScaled();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public String getPreviewMessage() {
        try {
            return bbjFontChooser.getPreviewMessage();
        } catch (BBjException e) {
            e.printStackTrace();
            return "";
        }
    }

    public Font getSelectedFont() {
        try {
            return (Font) bbjFontChooser.getSelectedFont();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public FontChooser setApproveButtonText(String text) {
        try {
            bbjFontChooser.setApproveButtonText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser setCancelButtonText(String text) {
        try {
            bbjFontChooser.setCancelButtonText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser setControlButtonsAreShown(boolean show) {
        try {
            bbjFontChooser.setControlButtonsAreShown(show);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser setFontsScaled(boolean scale) {
        try {
            bbjFontChooser.setFontsScaled(scale);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser setPreviewMessage(String message) {
        try {
            bbjFontChooser.setPreviewMessage(message);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser setSelectedFont(Font font) {
        try {
            bbjFontChooser.setSelectedFont((BBjFont) font);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public FontChooser onFontChooserApprove(Consumer<FontChooserApproveEvent> callback) {
        if (this.fontChooserApproveEventSink == null)
            this.fontChooserApproveEventSink = new FontChooserApproveEventSink(this, callback);
        else this.fontChooserApproveEventSink.addCallback(callback);
        return this;
    }

    public FontChooser onFontChooserCancel(Consumer<FontChooserCancelEvent> callback) {
        if (this.fontChooserCancelEventSink == null)
            this.fontChooserCancelEventSink = new FontChooserCancelEventSink(this, callback);
        else this.fontChooserCancelEventSink.addCallback(callback);
        return this;
    }

    public FontChooser onFontChooserChange(Consumer<FontChooserChangeEvent> callback) {
        if (this.fontChooserChangeEventSink == null)
            this.fontChooserChangeEventSink = new FontChooserChangeEventSink(this, callback);
        else this.fontChooserChangeEventSink.addCallback(callback);
        return this;
    }

    public FontChooser setText(String text) {
        super.setControlText(text);
        return this;
    }

    public FontChooser setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public FontChooser setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public FontChooser setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public FontChooser setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public FontChooser setID(String id){
        super.setControlID(id);
        return this;
    }

    public FontChooser setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public FontChooser addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public FontChooser removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
