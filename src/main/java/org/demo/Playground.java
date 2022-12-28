package org.demo;

import org.dwcj.App;
import org.dwcj.annotations.AppDarkTheme;
import org.dwcj.annotations.AppMeta;
import org.dwcj.annotations.AppTheme;
import org.dwcj.annotations.AppTitle;
import org.dwcj.annotations.MetaAttribute;
import org.dwcj.exceptions.DwcException;

@AppTitle("22 Hyyan ABo Fakher")
@AppTheme("system")
@AppDarkTheme("light")
@AppMeta(name = "author", content = "Hyyan Abo Fakher 2")
@AppMeta(name = "description", content = "This is a test app")
@AppMeta(name = "keywords", content = "test, app")
@AppMeta(name="viewport", content="width=device-width,initial-scale=1,user-scalable=no,minimum-scale=1,maximum-scale=1")
@AppMeta(name = "custom name", content = "custom content", attributes = {
    @MetaAttribute(name = "custom-attribute", value = "custom attribute value"),
    @MetaAttribute(name = "custom-attribute2", value = "custom attribute value2")
})
public class Playground extends App {

  @Override
  public void run() throws DwcException {
    msgbox("this is a test");
  }
}
