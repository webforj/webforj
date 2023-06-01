package org.dwcj.component.htmledit;


import com.basis.bbj.proxies.sysgui.BBjHtmlEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.basis.startup.type.sysgui.BBjColor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.dwcj.Environment;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.ModifyEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.htmledit.event.PageLoadedEvent;
import org.dwcj.component.htmledit.event.StateChangeEvent;
import org.dwcj.component.htmledit.sink.PageLoadedEventSink;
import org.dwcj.component.htmledit.sink.StateChangeEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/** A htmlEdit object. */
public final class HtmlEdit extends AbstractDwcComponent
    implements HasFocus, HasEnable, TabTraversable {

  private EventDispatcher dispatcher = new EventDispatcher();
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private BlurEventSink blurEventSink;
  private FocusEventSink focusEventSink;
  private PageLoadedEventSink pageLoadedEventSink;
  private StateChangeEventSink stateChangeEventSink;
  private ModifyEventSink modifyEventSink;


  private boolean isBasicToolbar = false;
  private List<String> basicToolBarStyles;
  private String locale = "";
  private String plainText = "";
  private String spellCheckLanguage = "";
  private HashMap<String, Boolean> states = new HashMap<String, Boolean>();
  private boolean isSpellChecked = false;

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
   * Adds a MouseEnter event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (getBBjControl() != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see HtmlEdit#addMouseEnterListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a MouseEnter event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (getBBjControl() != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (getBBjControl() != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @see HtmlEdit#addMouseExitListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a MouseExit event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (getBBjControl() != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (getBBjControl() != null
        && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see HtmlEdit#addRightMouseDownListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a RightMouseDown event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (getBBjControl() != null
        && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a focus event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addFocusListener(EventListener<FocusEvent> listener) {
    if (this.control != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see HtmlEdit#addFocusListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.control != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addBlurListener(EventListener<BlurEvent> listener) {
    if (this.control != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see HtmlEdit#addBlurListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }



  /**
   * Removes a blur event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.control != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a PageLoaded event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addPageLoadedListener(EventListener<PageLoadedEvent> listener) {
    if (this.control != null && this.dispatcher.getListenersCount(PageLoadedEvent.class) == 0) {
      this.pageLoadedEventSink.setCallback();
    }
    dispatcher.addEventListener(PageLoadedEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addPageLoadedListener method.
   *
   * @see HtmlEdit#addPageLoadedListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onPageLoaded(EventListener<PageLoadedEvent> listener) {
    return addPageLoadedListener(listener);
  }

  

  /**
   * Removes a PageLoaded event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removePageLoadedListener(EventListener<PageLoadedEvent> listener) {
    dispatcher.removeEventListener(PageLoadedEvent.class, listener);
    if (this.control != null && this.dispatcher.getListenersCount(PageLoadedEvent.class) == 0) {
      this.pageLoadedEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a StateChange event for the HtmlEdit component.
   *
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit addStateChangeListener(EventListener<StateChangeEvent> listener) {
    if (this.control != null && this.dispatcher.getListenersCount(StateChangeEvent.class) == 0) {
      this.stateChangeEventSink.setCallback();
    }
    dispatcher.addEventListener(StateChangeEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addStateChangeListener method.
   *
   * @see HtmlEdit#addStateChangeListener(EventListener)
   * @param listener the event listener to be added
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit onStateChange(EventListener<StateChangeEvent> listener) {
    return addStateChangeListener(listener);
  }

  

  /**
   * Removes a StateChange event from the HtmlEdit component.
   *
   * @param listener the event listener to be removed
   * @return The HtmlEdit itself
   */
  @ExcludeFromJacocoGeneratedReport
  public HtmlEdit removeStateChangeListener(EventListener<StateChangeEvent> listener) {
    dispatcher.removeEventListener(StateChangeEvent.class, listener);
    if (this.control != null && this.dispatcher.getListenersCount(StateChangeEvent.class) == 0) {
      this.stateChangeEventSink.removeCallback();
    }
    return this;
  }

  /**
   * This method gets a List of strings that specifies all available styles in the HtmlEdit toolbar.
   *
   * @return List of strings that specifies all available styles in the HtmlEdit toolbar.
   */
  public List<String> getAllToolbarStyles() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getAllToolbarStyles();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return List.of();
  }

  /**
   * This method gets the list of available spell-check languages for a HtmlEdit control.
   *
   * @return Returns a List of strings of available spell-check languages for a HtmlEdit control.
   */
  public List<String> getAvailableSpellCheckLanguages() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getAvailableSpellCheckLanguages();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return List.of();
  }

  /**
   * This method gets a List of strings that specifies all available editor states in the HTMLEditor
   * toolbar.
   *
   * @return Returns a list of strings that specifies all available editor stattes in the HTMLEditor
   *         toolbar.
   */
  public List<String> getAvailableStates() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getAvailableStates();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return List.of();
  }


  /**
   * This method gets a boolean that indicates whether this HtmlEdit control is currently set to use
   * a basic toolbar.
   *
   * @return Returns a boolean that indicates whether this HtmlEdit control is currently set to use
   *         a basic toolbar .
   */
  public boolean isBasicToolbar() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getBasicToolbar();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return this.isBasicToolbar;
  }

  /**
   * This method gets a List of strings that specifies the styles to be included when the HtmlEdit
   * basic toolbar is selected.
   *
   * @return Returns a List of strings that specifies the styles to be included when the HtmlEdit
   *         basic toolbar is selected.
   */
  public List<String> getBasicToolbarStyles() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getBasicToolbarStyles();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return this.basicToolBarStyles;
  }

  /**
   * This method gets a string representing the client type for this HTMLEdit control.
   *
   * @return Returns a string representing the client type for the HtmlEdit control ("Browser" (BUI
   *         or DWC), "Swing" (basic HTML 3.2), "JavaFX" (WebKit), "Chromium").
   */
  public String getClientType() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getClientType();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return "";
  }

  /**
   * This method gets the client version for this HtmlEdit control.
   *
   * @return Returns the version of the HtmlEdit control - the client versions will change over
   *         time.
   */
  public String getClientVersion() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getClientVersion();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return "";
  }

  /**
   * This method gets the UI locale of an HTMLEdit control.
   *
   * @return A string representing the UI locale of the HTMLEdit control.
   */
  public String getLocale() {
    if (getBBjControl() != null) {

      return getBBjControl().getLocale();

    }
    return this.locale;
  }

  /**
   * This method gets the list of available UI Locales for an HTMLEdit control.
   *
   * @return a List of strings representing the available UI locales for an HTMLEdit control.
   */
  public List<String> getLocales() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getLocales();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    return List.of();

  }

  /**
   * This method gets the plain text content of an HTMLEditor control, without any HTML markup.
   *
   * @return A string representing only the plaintext content of the HTML control.
   */
  public String getPlainText() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getPlainText();
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    return this.plainText;
  }

  /**
   * This method gets the spell-check language of an HTMLEdit control.
   *
   * @return A string representing the spell-check language of an HTMLEdit control.
   */
  public String getSpellCheckLanguage() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getSpellCheckLanguage();
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    return this.spellCheckLanguage;
  }

  /**
   * This method gets the Boolean value of a specified state in the HtmlEdit toolbar.
   *
   * @param state specifying one of the state names from HTMLEdit::getAvailableStates
   * @return Boolean value of the specified state in the HtmlEdit toolbar.
   */
  public boolean getState(String state) {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().getState(state);
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    return states.get(state);
  }

  /**
   * This method gets a boolean that indicates whether spell-checking is currently enabled on this
   * HTMLEdit control.
   *
   * @return a boolean representing whether or not spell-check is enabled on this control.
   */
  public boolean isSpellChecked() {
    if (getBBjControl() != null) {
      try {
        return getBBjControl().isSpellChecked();
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }

    return this.isSpellChecked;
  }

  /**
   * This method specifies whether this HTMLEdit control should use a basic toolbar.
   *
   * @param basicToolbar - Boolean representing whether or not to use a basic toolbar, true for yes,
   *        false for no
   * @return Returns this
   */
  public HtmlEdit setBasicToolbar(boolean basicToolbar) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setBasicToolbar(basicToolbar);
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    this.isBasicToolbar = basicToolbar;
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
    if (getBBjControl() != null) {
      try {
        getBBjControl().setBasicToolbarStyles((BBjVector) styles);
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    this.basicToolBarStyles = styles;
    return this;
  }

  /**
   * Sets the UI locale of the HTMLEdit control.
   *
   * @param locale - A String representing the locale you wish to set the HTMLEdit to.
   * @return Returns this
   */
  public HtmlEdit setLocale(String locale) {
    if (getBBjControl() != null) {
      getBBjControl().setLocale(locale);
    }
    this.locale = locale;
    return this;
  }

  /**
   * Sets the text of an HtmlEdit control.
   *
   * @param text - A String representing the text you wish to set.
   * @return Returns this
   */
  public HtmlEdit setPlainText(String text) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setPlainText(text);
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    this.plainText = text;
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
    if (getBBjControl() != null) {
      try {
        getBBjControl().setState(state, value);
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    this.states.put(state, value);
    return this;
  }

  /**
   * Specifies whether or not spell checking should be enabled in this HtmlEdit control.
   *
   * @param spellChecked - boolean value - true enables spell checking, false disables it.
   * @return Returns this
   */
  public HtmlEdit setSpellChecked(boolean spellChecked) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setSpellChecked(spellChecked);
      } catch (BBjException e) {
        throw new DwcjRuntimeException();
      }
    }
    this.isSpellChecked = spellChecked;
    return this;
  }

  /**
   * Sets the spell check language for this HtmlEdit control.
   *
   * @param language - String representing the desired language for spell checking
   * @return Returns this
   */
  public HtmlEdit setSpellCheckLanguage(String language) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setSpellCheckLanguage(language);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.spellCheckLanguage = language;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit focus() {
    super.focusComponent();
    return this;
  }

  /**
   * {@inheritDoc}
   */
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

  /**
   * {@inheritDoc}
   */
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


  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HtmlEdit removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  public HtmlEdit setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }



  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    if (this.isBasicToolbar) {
      this.setBasicToolbar(this.isBasicToolbar);
    }

    if (!this.basicToolBarStyles.isEmpty()) {
      this.setBasicToolbarStyles(this.basicToolBarStyles);
    }

    if (!this.locale.equals("")) {
      this.setLocale(this.locale);
    }

    if (!this.plainText.equals("")) {
      this.setPlainText(this.plainText);
    }

    if (!this.spellCheckLanguage.equals("")) {
      this.setSpellCheckLanguage(this.spellCheckLanguage);
    }

    if (!this.states.isEmpty()) {
      for (Map.Entry<String, Boolean> entry : this.states.entrySet()) {
        this.setState(entry.getKey(), entry.getValue());
      }
    }

    if (this.isSpellChecked) {
      this.setSpellChecked(this.isSpellChecked);
    }
  }

}

