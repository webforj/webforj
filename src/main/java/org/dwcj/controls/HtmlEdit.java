package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjHtmlEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.List;

public final  class HtmlEdit extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjHtmlEdit bbjHtmlEdit;


    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visbility flag
            ctrl = w.addHtmlEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            bbjHtmlEdit = (BBjHtmlEdit) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public List<String> getAllToolbarStyles() {
        try {
            return bbjHtmlEdit.getAllToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    public List<String> getAvailableSpellCheckLanguages() {
        try {
            return bbjHtmlEdit.getAvailableSpellCheckLanguages();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    public List<String> getAvailableStates() {
        try {
            return bbjHtmlEdit.getAvailableStates();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    public boolean getBasicToolbar() {
        try {
            return bbjHtmlEdit.getBasicToolbar();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public List<String> getBasicToolbarStyles() {
        try {
            return bbjHtmlEdit.getBasicToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    public String getClientType() {
        try {
            return bbjHtmlEdit.getClientType();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getClientVersion() {
        try {
            return bbjHtmlEdit.getClientVersion();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getLocale() {
        return bbjHtmlEdit.getLocale();
    }

    public List<String> getLocales() {
        try {
            return bbjHtmlEdit.getLocales();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    public String getPlainText() {
        try {
            return bbjHtmlEdit.getPlainText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getSpellCheckLanguage() {
        try {
            return bbjHtmlEdit.getSpellCheckLanguage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean getState(String state) {
        try {
            return bbjHtmlEdit.getState(state);
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean isSpellChecked() {
        try {
            return bbjHtmlEdit.isSpellChecked();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public void setBasicToolbar(boolean basicToolbar) {
        try {
            bbjHtmlEdit.setBasicToolbar(basicToolbar);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setBasicToolbarStyles(ArrayList styles) {
        try {
            bbjHtmlEdit.setBasicToolbarStyles((BBjVector) styles);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLocale(String locale) {
        bbjHtmlEdit.setLocale(locale);
    }

    public void setPlainText(String text) {
        try {
            bbjHtmlEdit.setPlainText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setState(String state, boolean value) {
        try {
            bbjHtmlEdit.setState(state, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSpellChecked(boolean spellChecked) {
        try {
            bbjHtmlEdit.setSpellChecked(spellChecked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSpellCheckLanguage(String language) {
        try {
            bbjHtmlEdit.setSpellCheckLanguage(language);
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
