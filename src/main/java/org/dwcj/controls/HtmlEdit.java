package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjHtmlEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;

public final  class HtmlEdit extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjHtmlEdit htmlEdit;

    public HtmlEdit() {}

    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visbility flag
            ctrl = w.addHtmlEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            htmlEdit = (BBjHtmlEdit) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public ArrayList getAllToolbarStyles() {
        try {
            return (ArrayList) htmlEdit.getAllToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public ArrayList getAvailableSpellCheckLanguages() {
        try {
            return (ArrayList) htmlEdit.getAvailableSpellCheckLanguages();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public ArrayList getAvailableStates() {
        try {
            return (ArrayList) htmlEdit.getAvailableStates();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean getBasicToolbar() {
        try {
            return htmlEdit.getBasicToolbar();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public ArrayList getBasicToolbarStyles() {
        try {
            return (ArrayList) htmlEdit.getBasicToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getClientType() {
        try {
            return htmlEdit.getClientType();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getClientVersion() {
        try {
            return htmlEdit.getClientVersion();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getLocale() {
        return htmlEdit.getLocale();
    }

    public ArrayList getLocales() {
        try {
            return (ArrayList) htmlEdit.getLocales();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getPlainText() {
        try {
            return htmlEdit.getPlainText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getSpellCheckLanguage() {
        try {
            return htmlEdit.getSpellCheckLanguage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean getState(String state) {
        try {
            return htmlEdit.getState(state);
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean isSpellChecked() {
        try {
            return htmlEdit.isSpellChecked();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public void setBasicToolbar(boolean basicToolbar) {
        try {
            htmlEdit.setBasicToolbar(basicToolbar);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setBasicToolbarStyles(ArrayList styles) {
        try {
            htmlEdit.setBasicToolbarStyles((BBjVector) styles);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLocale(String locale) {
        htmlEdit.setLocale(locale);
    }

    public void setPlainText(String text) {
        try {
            htmlEdit.setPlainText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setState(String state, boolean value) {
        try {
            htmlEdit.setState(state, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSpellChecked(boolean spellChecked) {
        try {
            htmlEdit.setSpellChecked(spellChecked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSpellCheckLanguage(String language) {
        try {
            htmlEdit.setSpellCheckLanguage(language);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public IExpansible setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
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

    @Override
    public IThemable setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
