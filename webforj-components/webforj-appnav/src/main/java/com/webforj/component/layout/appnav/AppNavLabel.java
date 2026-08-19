package com.webforj.component.layout.appnav;

import com.webforj.component.Component;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasPrefixAndSuffix;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;

/**
 * A non interactive section label for the application navigator {@link AppNav}.
 *
 * <p>
 * A label is placed between the top level items of the navigator and titles the run of following
 * items up to the next label or the end of the menu. The navigator hides a label automatically when
 * its section has no visible items, for example when a search filters them all out or when all of
 * them are pinned away.
 * </p>
 *
 * <p>
 * A label is composed into the navigator like any other component, the order of the calls defines
 * the sections. For example {@code nav.add(new AppNavLabel("Management"))} followed by
 * {@code nav.addItem(users)}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 *
 * @see AppNav
 */
@NodeName("dwc-app-nav-label")
public class AppNavLabel extends ElementComposite
    implements HasText<AppNavLabel>, HasStyle<AppNavLabel>, HasClassName<AppNavLabel>,
    HasAttribute<AppNavLabel>, HasVisibility<AppNavLabel>, HasPrefixAndSuffix<AppNavLabel> {
  private Component prefix;
  private Component suffix;

  private static final String SLOT_PREFIX = "prefix";
  private static final String SLOT_SUFFIX = "suffix";

  /**
   * Constructs a new label.
   */
  public AppNavLabel() {
    super();
  }

  /**
   * Constructs a new label with the given text.
   *
   * @param text the text of the label
   */
  public AppNavLabel(String text) {
    this();
    setText(text);
  }

  /**
   * Constructs a new label with the given text and prefix.
   *
   * @param text the text of the label
   * @param prefix the prefix component of the label
   */
  public AppNavLabel(String text, Component prefix) {
    this(text);

    if (prefix != null) {
      setPrefixComponent(prefix);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppNavLabel setPrefixComponent(Component prefix) {
    if (prefix.equals(this.prefix)) {
      return this;
    }

    if (this.prefix != null) {
      this.prefix.destroy();
    }

    this.prefix = prefix;
    getElement().add(SLOT_PREFIX, prefix);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getPrefixComponent() {
    return prefix;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppNavLabel setSuffixComponent(Component suffix) {
    if (suffix.equals(this.suffix)) {
      return this;
    }

    if (this.suffix != null) {
      this.suffix.destroy();
    }

    this.suffix = suffix;
    getElement().add(SLOT_SUFFIX, suffix);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getSuffixComponent() {
    return suffix;
  }
}
