package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.nio.charset.StandardCharsets;

public final class StringEditBox extends AbstractDwcControl implements IReadOnly{

    String mask;

    private BBjInputE bbjInputE;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }
    
    public StringEditBox() {}

    public StringEditBox(String text) {
        setText(text);
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputE(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            bbjInputE = (BBjInputE) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public StringEditBox setMask(String mask){
        if (ctrl != null){
        try {
            ((BBjInputE)ctrl).setMask(mask);
        } catch (BBjException e) {
            App.consoleLog(e.getMessage());
            throw new RuntimeException(e);
        }
        }
        this.mask = mask;
        return this;
    }

    public int getError() {
        try {
            return bbjInputE.getError();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getEditString() {
        try {
            return new String(bbjInputE.getEditString(), StandardCharsets.UTF_8);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean isHighlight() {
        try {
            return bbjInputE.getHighlight();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean isInsertMode() {
        try {
            return bbjInputE.getInsertMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getLength() {
        try {
            bbjInputE.getLength();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getMask() {
        try {
            return bbjInputE.getMask();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public int getMargin() {
        try {
            return bbjInputE.getMargin();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getPadCharacter() {
        try {
            return bbjInputE.getPadCharacter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean isPassEnter() {
        try {
            return bbjInputE.getPassEnter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean isPassTab() {
        try {
            return bbjInputE.getPassTab();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public String getRestore() {
        try {
            return bbjInputE.getRestore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    @Override
    public Boolean isReadOnly() {
        try {
            return bbjInputE.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void restore() {
        try {
            bbjInputE.restore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public StringEditBox selectAll() {
        try {
            bbjInputE.selectAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public StringEditBox setReadOnly(boolean editable) {
        try {
            bbjInputE.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setEditString(String edit) {
        try {
            bbjInputE.setEditString(edit.getBytes(StandardCharsets.UTF_8));
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setHighlight(boolean highlight) {
        try {
            bbjInputE.setHighlight(highlight);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setInsertMode(boolean insert) {
        try {
            bbjInputE.setInsertMode(insert);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setLength(int len) {
        try {
            bbjInputE.setLength(len);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setMargin(int marginWidth) {
        try {
            bbjInputE.setMargin(marginWidth);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setPadCharacter(String pad) {
        try {
            bbjInputE.setPadCharacter(pad);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setPassEnter(boolean pass) {
        try {
            bbjInputE.setPassEnter(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setPassTab(boolean pass) {
        try {
            bbjInputE.setPassTab(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setRestore(String restore) {
        try {
            bbjInputE.setRestore(restore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public StringEditBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public StringEditBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public StringEditBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public StringEditBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    public StringEditBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public StringEditBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();
        if (this.mask != null)
            setMask(this.mask);
    }

    @Override
    public StringEditBox setID(String id){
        super.setID(id);
        return this;
    }
}
