package org.dwcj.component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.dwcj.component.window.AbstractWindow;

public class AbstractDwcComponentMock extends AbstractDwcComponent
    implements HasExpanse<AbstractDwcComponentMock, Expanse>, HasReadOnly {

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
    return new ArrayList<>(Arrays.asList("expanse", "readonly", "doesNotExist"));
  }

  @Override
  protected void create(AbstractWindow panel) {
    throw new UnsupportedOperationException("Unimplemented method 'create'");
  }
}
