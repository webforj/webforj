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

    /**
     * This method returns a List of strings that specifies all available styles in the HtmlEdit toolbar.
     * @return List of strings that specifies all available styles in the HtmlEdit toolbar.
     */
    public List<String> getAllToolbarStyles() {
        try {
            return bbjHtmlEdit.getAllToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns the list of available spell-check languages for a HtmlEdit control.
     *
     * @return Returns a List of strings of available spell-check languages for a HtmlEdit control.
     */
    public List<String> getAvailableSpellCheckLanguages() {
        try {
            return bbjHtmlEdit.getAvailableSpellCheckLanguages();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns a List of strings that specifies all available editor states in the HTMLEditor toolbar.
     * 
     * @return Returns a list of strings that specifies all available editor stattes in the HTMLEditor toolbar.
     */
    public List<String> getAvailableStates() {
        try {
            return bbjHtmlEdit.getAvailableStates();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }


    /**
     * This method returns a boolean that indicates whether this HtmlEdit control is currently set to use a basic toolbar.
     * 
     * @return Returns a boolean that indicates whether this HtmlEdit control is currently set to use a basic toolbar .
     */
    public boolean getBasicToolbar() {
        try {
            return bbjHtmlEdit.getBasicToolbar();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * This method returns a List of strings that specifies the styles to be included when the HtmlEdit basic toolbar is selected.
     * 
     * @return Returns a List of strings that specifies the styles to be included when the HtmlEdit basic toolbar is selected.
     */
    public List<String> getBasicToolbarStyles() {
        try {
            return bbjHtmlEdit.getBasicToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns a string representing the client type for this HTMLEdit control.
     * 
     * @return Returns a string representing the client type for the HtmlEdit control ("Browser" (BUI or DWC), "Swing" (basic HTML 3.2), "JavaFX" (WebKit), "Chromium").
     */
    public String getClientType() {
        try {
            return bbjHtmlEdit.getClientType();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the client version for this BBjHtmlEdit control.
     * @return Returns the version of the HtmlEdit control - the client versions will change over time. 
     */
    public String getClientVersion() {
        try {
            return bbjHtmlEdit.getClientVersion();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the UI locale of an HTMLEdit control.
     * @return A string representing the UI locale of the HTMLEdit control.
     */
    public String getLocale() {
        return bbjHtmlEdit.getLocale();
    }

    /**
     *  This method returns the list of available UI Locales for an HTMLEdit control.
     * @return a List of strings representing the available UI locales for an HTMLEdit control.
     */
    public List<String> getLocales() {
        try {
            return bbjHtmlEdit.getLocales();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns the plain text content of an HTMLEditor control, without any HTML markup.
     * @return A string representing only the plaintext content of the HTML control.
     */
    public String getPlainText() {
        try {
            return bbjHtmlEdit.getPlainText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the spell-check language of an HTMLEdit control.
     * @return A string representing the spell-check language of an HTMLEdit control.
     */
    public String getSpellCheckLanguage() {
        try {
            return bbjHtmlEdit.getSpellCheckLanguage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the Boolean value of a specified state in the HtmlEdit toolbar.
     * @param String specifying one of the state names from HTMLEdit::getAvailableStates
     * @return Boolean value of the specified state in the HtmlEdit toolbar.
     */
    public boolean getState(String state) {
        try {
            return bbjHtmlEdit.getState(state);
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * This method returns a boolean that indicates whether spell-checking is currently enabled on this HTMLEdit control.
     * @return a boolean representing whether or not spell-check is enabled on this control.
     */
    public boolean isSpellChecked() {
        try {
            return bbjHtmlEdit.isSpellChecked();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * This method specifies whether this HTMLEdit control should use a basic toolbar
     * @param basicToolbar - Boolean representing whether or not to use a basic toolbar, true for yes, false for no
     */
    public void setBasicToolbar(boolean basicToolbar) {
        try {
            bbjHtmlEdit.setBasicToolbar(basicToolbar);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * This method specifies the styles to be included when the HTMLEdit basic toolbar is used
     * @param styles - A List of styles to be shown when the basic toolbar is selected, should be an array of Strings
     */
    public void setBasicToolbarStyles(ArrayList<String> styles) {
        try {
            bbjHtmlEdit.setBasicToolbarStyles((BBjVector) styles);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * 
     * @param locale
     */
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

