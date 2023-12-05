package org.dwcj.component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.dwcj.component.window.Window;
import org.dwcj.concern.HasExpanse;
import org.dwcj.concern.HasHighlightOnFocus;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.HasReadOnly;
import org.dwcj.concern.HasTheme;

public class DwcComponentMock extends DwcComponent<DwcComponentMock>
    implements HasExpanse<DwcComponentMock, Expanse>, HasTheme<DwcComponentMock, Theme>,
    HasReadOnly<DwcComponentMock>, HasHighlightOnFocus<DwcComponentMock>,
    HasHorizontalAlignment<DwcComponentMock> {
  @Override
  public DwcComponentMock setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return this;
  }

  @Override
  public Expanse getExpanse() {
    return getComponentExpanse();
  }

  @Override
  public DwcComponentMock setTheme(Theme theme) {
    setComponentTheme(theme);
    return this;
  }

  @Override
  public Theme getTheme() {
    return getComponentTheme();
  }

  @Override
  public DwcComponentMock setReadOnly(boolean readonly) {
    setComponentReadOnly(readonly);
    return this;
  }

  @Override
  public boolean isReadOnly() {
    return isComponentReadOnly();
  }

  @Override
  public List<String> getRestrictedProperties() {
    return new ArrayList<>(Arrays.asList("expanse", "doesNotExist"));
  }

  @Override
  protected void onCreate(Window panel) {
    // pass
  }

  @Override
  public HasHighlightOnFocus.Behavior getHighlightOnFocus() {
    return getComponentHighlightOnFocus();
  }

  @Override
  public DwcComponentMock setHighlightOnFocus(HasHighlightOnFocus.Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return this;
  }

  public DwcComponentMock setDefaultHorizontalAlignment(Alignment alignment) {
    setComponentDefaultHorizontalAlignment(alignment);
    return this;
  }

  @Override
  public DwcComponentMock setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return this;
  }

  @Override
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }
}
