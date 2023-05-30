package org.dwcj.component.htmledit;


import com.basis.bbj.proxies.sysgui.BBjHtmlEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.ArrayList;
import java.util.List;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/** A htmlEdit object. */
public final class HtmlEdit extends AbstractDwcComponent
    implements HasFocus, HasEnable, TabTraversable {

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }


  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      this.setControl(w.addHtmlEdit(this.getText(), flags));
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  private BBjHtmlEdit getBBjControl() {
    try {
      return (BBjHtmlEdit) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * This method gets a List of strings that specifies all available styles in the HtmlEdit toolbar.
   *
   * @return List of strings that specifies all available styles in the HtmlEdit toolbar.
   */
  public List<String> getAllToolbarStyles() {
    try {
      return getBBjControl().getAllToolbarStyles();
    } catch (BBjException e) {
      Environment.logError(e);
      return new ArrayList<>();
    }
  }

  /**
   * This method gets the list of available spell-check languages for a HtmlEdit control.
   *
   * @return Returns a List of strings of available spell-check languages for a HtmlEdit control.
   */
  public List<String> getAvailableSpellCheckLanguages() {
    try {
      return getBBjControl().getAvailableSpellCheckLanguages();
    } catch (BBjException e) {
      Environment.logError(e);
      return new ArrayList<>();
    }
  }

  /**
   * This method gets a List of strings that specifies all available editor states in the HTMLEditor
   * toolbar.
   *
   * @return Returns a list of strings that specifies all available editor stattes in the HTMLEditor
   *         toolbar.
   */
  public List<String> getAvailableStates() {
    try {
      return getBBjControl().getAvailableStates();
    } catch (BBjException e) {
      Environment.logError(e);
      return new ArrayList<>();
    }
  }


  /**
   * This method gets a boolean that indicates whether this HtmlEdit control is currently set to use
   * a basic toolbar.
   *
   * @return Returns a boolean that indicates whether this HtmlEdit control is currently set to use
   *         a basic toolbar .
   */
  public boolean isBasicToolbar() {
    try {
      return getBBjControl().getBasicToolbar();
    } catch (BBjException e) {
      Environment.logError(e);
      return false;
    }
  }

  /**
   * This method gets a List of strings that specifies the styles to be included when the HtmlEdit
   * basic toolbar is selected.
   *
   * @return Returns a List of strings that specifies the styles to be included when the HtmlEdit
   *         basic toolbar is selected.
   */
  public List<String> getBasicToolbarStyles() {
    try {
      return getBBjControl().getBasicToolbarStyles();
    } catch (BBjException e) {
      Environment.logError(e);
      return new ArrayList<>();
    }
  }

  /**
   * This method gets a string representing the client type for this HTMLEdit control.
   *
   * @return Returns a string representing the client type for the HtmlEdit control ("Browser" (BUI
   *         or DWC), "Swing" (basic HTML 3.2), "JavaFX" (WebKit), "Chromium").
   */
  public String getClientType() {
    try {
      return getBBjControl().getClientType();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  /**
   * This method gets the client version for this HtmlEdit control.
   *
   * @return Returns the version of the HtmlEdit control - the client versions will change over
   *         time.
   */
  public String getClientVersion() {
    try {
      return getBBjControl().getClientVersion();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  /**
   * This method gets the UI locale of an HTMLEdit control.
   *
   * @return A string representing the UI locale of the HTMLEdit control.
   */
  public String getLocale() {
    return getBBjControl().getLocale();
  }

  /**
   * This method gets the list of available UI Locales for an HTMLEdit control.
   *
   * @return a List of strings representing the available UI locales for an HTMLEdit control.
   */
  public List<String> getLocales() {
    try {
      return getBBjControl().getLocales();
    } catch (BBjException e) {
      Environment.logError(e);
      return new ArrayList<>();
    }
  }

  /**
   * This method gets the plain text content of an HTMLEditor control, without any HTML markup.
   *
   * @return A string representing only the plaintext content of the HTML control.
   */
  public String getPlainText() {
    try {
      return getBBjControl().getPlainText();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  /**
   * This method gets the spell-check language of an HTMLEdit control.
   *
   * @return A string representing the spell-check language of an HTMLEdit control.
   */
  public String getSpellCheckLanguage() {
    try {
      return getBBjControl().getSpellCheckLanguage();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  /**
   * This method gets the Boolean value of a specified state in the HtmlEdit toolbar.
   *
   * @param state specifying one of the state names from HTMLEdit::getAvailableStates
   * @return Boolean value of the specified state in the HtmlEdit toolbar.
   */
  public boolean getState(String state) {
    try {
      return getBBjControl().getState(state);
    } catch (BBjException e) {
      Environment.logError(e);
      return false;
    }
  }

  /**
   * This method gets a boolean that indicates whether spell-checking is currently enabled on this
   * HTMLEdit control.
   *
   * @return a boolean representing whether or not spell-check is enabled on this control.
   */
  public boolean isSpellChecked() {
    try {
      return getBBjControl().isSpellChecked();
    } catch (BBjException e) {
      Environment.logError(e);
      return false;
    }
  }

  /**
   * This method specifies whether this HTMLEdit control should use a basic toolbar.
   *
   * @param basicToolbar - Boolean representing whether or not to use a basic toolbar, true for yes,
   *        false for no
   * @return Returns this
   */
  public HtmlEdit setBasicToolbar(boolean basicToolbar) {
    try {
      getBBjControl().setBasicToolbar(basicToolbar);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /**
   * This method specifies the styles to be included when the HTMLEdit basic toolbar is used.
   *
   * @param styles - A List of styles to be shown when the basic toolbar is selected, should be an
   *        array of Strings
   * @return Returns this
   */
  public HtmlEdit setBasicToolbarStyles(List<String> styles) {
    try {
      getBBjControl().setBasicToolbarStyles((BBjVector) styles);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /**
   * Sets the UI locale of the HTMLEdit control.
   *
   * @param locale - A String representing the locale you wish to set the HTMLEdit to.
   * @return Returns this
   */
  public HtmlEdit setLocale(String locale) {
    getBBjControl().setLocale(locale);
    return this;
  }

  /**
   * Sets the text of an HtmlEdit control.
   *
   * @param text - A String representing the text you wish to set.
   * @return Returns this
   */
  public HtmlEdit setPlainText(String text) {
    try {
      getBBjControl().setPlainText(text);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /**
   * Sets a specified state in the HtmlEdit toolbar to the specified boolean value.
   *
   * @param state - One of the state names from HtmlEdit::getAvailableStates()
   * @param value - Boolean value - true activates the specified state while false deactivates it.
   *        The null() method toggles it. Empty parameter is assumed null()
   * @return Returns this
   */
  public HtmlEdit setState(String state, boolean value) {
    try {
      getBBjControl().setState(state, value);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /**
   * Specifies whether or not spell checking should be enabled in this HtmlEdit control.
   *
   * @param spellChecked - boolean value - true enables spell checking, false disables it.
   * @return Returns this
   */
  public HtmlEdit setSpellChecked(boolean spellChecked) {
    try {
      getBBjControl().setSpellChecked(spellChecked);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  /**
   * Sets the spell check language for this HtmlEdit control.
   *
   * @param language - String representing the desired language for spell checking
   * @return Returns this
   */
  public HtmlEdit setSpellCheckLanguage(String language) {
    try {
      getBBjControl().setSpellCheckLanguage(language);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return this;
  }

  @Override
  public HtmlEdit focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
      try {
        return getBBjControl().isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public HtmlEdit setTabTraversable(Boolean traversable) {
    if (this.control != null) {
      try {
        getBBjControl().setTabTraversable(traversable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traversable;
    return this;
  }



  @Override
  public HtmlEdit setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public HtmlEdit setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public HtmlEdit setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public HtmlEdit setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public HtmlEdit setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public HtmlEdit setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public HtmlEdit addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public HtmlEdit removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  public HtmlEdit setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }



  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    super.catchUp();
  }

}

