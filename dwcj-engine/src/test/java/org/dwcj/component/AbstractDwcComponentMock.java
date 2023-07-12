package org.dwcj.component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.dwcj.component.window.AbstractWindow;

public class AbstractDwcComponentMock extends AbstractDwcComponent
    implements HasExpanse<AbstractDwcComponentMock, Expanse>,
    HasTheme<AbstractDwcComponentMock, Theme>, HasReadOnly,
    HighlightableOnFocus<AbstractDwcComponentMock>, HorizontalAlignment<AbstractDwcComponentMock> {

  @Override
  public AbstractDwcComponentMock setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return this;
  }

  @Override
  public Expanse getExpanse() {
    return (Expanse) getComponentExpanse();
  }

  @Override
  public AbstractDwcComponentMock setTheme(Theme theme) {
    setComponentTheme(theme);
    return this;
  }

  @Override
  public Theme getTheme() {
    return (Theme) getComponentTheme();
  }

  @Override
  public AbstractDwcComponentMock setReadOnly(Boolean readonly) {
    setComponentReadOnly(readonly);
    return this;
  }

  @Override
  public Boolean isReadOnly() {
    return isComponentReadOnly();
  }

  @Override
  public List<String> getRestrictedProperties() {
    return new ArrayList<>(Arrays.asList("expanse", "doesNotExist"));
  }

  @Override
  protected void create(AbstractWindow panel) {
    throw new UnsupportedOperationException("Unimplemented method 'create'");
  }

  @Override
  public HighlightableOnFocus.Behavior getHighlightOnFocus() {
    return getComponentHighlightOnFocus();
  }

  @Override
  public AbstractDwcComponentMock setHighlightOnFocus(HighlightableOnFocus.Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return this;
  }

  public AbstractDwcComponentMock setDefaultHorizontalAlignment(Alignment alignment) {
    setComponentDefaultHorizontalAlignment(alignment);
    return this;
  }

  @Override
  public AbstractDwcComponentMock setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return this;
  }

  @Override
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }
}
