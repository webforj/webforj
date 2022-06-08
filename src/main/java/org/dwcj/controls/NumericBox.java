package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class NumericBox extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjInputN numBox;

    public NumericBox() {}

    public NumericBox(String text) {
        setText(text);
    }

    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputN(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            numBox = (BBjInputN) this.ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String getCommaCharacter() {
        try {
            return numBox.getCommaCharacter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public String getDotCharacter() {
        try {
            return numBox.getDotCharacter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public String getEditString() {
        try {
            return new String(numBox.getEditString());
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public int getError() {
        try {
            return numBox.getError();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean getHighlight() {
        try {
            return numBox.getHighlight();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getInsertMode() {
        try {
            return numBox.getInsertMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getLength() {
        try {
            return numBox.getLength();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMargin() {
        try {
            return numBox.getMargin();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getMask() {
        try {
            return numBox.getMask();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean getNegatable() {
        try {
            return numBox.getNegateable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getPassEnter() {
        try {
            return numBox.getPassEnter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getPassTab() {
        try {
            return numBox.getPassTab();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public String getRestore() {
        try {
            return numBox.getRestore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public boolean getUseEditCommas() {
        try {
            return numBox.getUseEditCommas();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public float getValue() {
        try {
            return numBox.getValue().floatValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean isEditable() {
        try {
            return numBox.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void restore() {
        try {
            numBox.restore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void selectAll() {
        try {
            numBox.selectAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setCommaCharacter(String comma) {
        try {
            numBox.setCommaCharacter(comma);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setDotCharacter(String dot) {
        try {
            numBox.setDotCharacter(dot);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditable(boolean editable) {
        try {
            numBox.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditString(String edit) {
        try {
            numBox.setEditString(edit.getBytes());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setHighlight(boolean highlight) {
        try {
            numBox.setHighlight(highlight);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setInsertMode(boolean insert) {
        try {
            numBox.setInsertMode(insert);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLength(int len) {
        try {
            numBox.setLength(len);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMargin(int marginWidth) {
        try {
            numBox.setMargin(marginWidth);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMask(String mask) {
        try {
            numBox.setMask(mask);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setNegatable(boolean negatable) {
        try {
            numBox.setNegateable(negatable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPassEnter(boolean pass) {
        try {
            numBox.setPassEnter(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPassTab(boolean pass) {
        try {
            numBox.setPassEnter(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setRestore(float restore) {
        try {
            numBox.setRestore(String.valueOf(restore));
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setUseEditCommas(boolean useCommas) {
        try {
            numBox.setUseEditCommas(useCommas);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setValue(float value) {
        try {
            numBox.setValue(BasisNumber.createBasisNumber(value));
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public NumericBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public NumericBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public NumericBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public NumericBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public NumericBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public NumericBox setText(String text) {
        super.setText(text);
        return this;
    }
}
