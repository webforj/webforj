package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.nio.charset.StandardCharsets;

public final class StringEditBox extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    String mask;

    private BBjInputE stringEditBox;

    public StringEditBox() {}

    public StringEditBox(String text) {
        setText(text);
    }

    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputE(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            stringEditBox = (BBjInputE) ctrl;
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
            return stringEditBox.getError();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getEditString() {
        try {
            return new String(stringEditBox.getEditString(), StandardCharsets.UTF_8);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean getHighlight() {
        try {
            return stringEditBox.getHighlight();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getInsertMode() {
        try {
            return stringEditBox.getInsertMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getLength() {
        try {
            stringEditBox.getLength();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getMask() {
        try {
            return stringEditBox.getMask();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public int getMargin() {
        try {
            return stringEditBox.getMargin();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getPadCharacter() {
        try {
            return stringEditBox.getPadCharacter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean getPassEnter() {
        try {
            return stringEditBox.getPassEnter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getPassTab() {
        try {
            return stringEditBox.getPassTab();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public String getRestore() {
        try {
            return stringEditBox.getRestore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean isEditable() {
        try {
            return stringEditBox.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void restore() {
        try {
            stringEditBox.restore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void selectAll() {
        try {
            stringEditBox.selectAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditable(boolean editable) {
        try {
            stringEditBox.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditString(String edit) {
        try {
            stringEditBox.setEditString(edit.getBytes(StandardCharsets.UTF_8));
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setHighlight(boolean highlight) {
        try {
            stringEditBox.setHighlight(highlight);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setInsertMode(boolean insert) {
        try {
            stringEditBox.setInsertMode(insert);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLength(int len) {
        try {
            stringEditBox.setLength(len);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMargin(int marginWidth) {
        try {
            stringEditBox.setMargin(marginWidth);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPadCharacter(String pad) {
        try {
            stringEditBox.setPadCharacter(pad);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPassEnter(boolean pass) {
        try {
            stringEditBox.setPassEnter(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPassTab(boolean pass) {
        try {
            stringEditBox.setPassTab(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setRestore(String restore) {
        try {
            stringEditBox.setRestore(restore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
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

    @Override
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
}
