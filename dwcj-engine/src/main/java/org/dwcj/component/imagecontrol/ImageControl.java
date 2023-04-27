package org.dwcj.component.imagecontrol;

import com.basis.bbj.funcs.Env;
import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.bbj.proxies.sysgui.BBjImageCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.window.AbstractWindow;

import java.awt.*;
import java.io.IOException;
import java.util.Base64;

public final class ImageControl extends AbstractDwcComponent {

  private BBjImageCtrl bbjImageControl;

  private static final String CLEARPIXEL =
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z/C/HgAGgwJ/lK3Q6wAAAABJRU5ErkJggg==";

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      BBjImage img = Environment.getInstance().getSysGui().getImageManager()
          .loadImageFromBytes(Base64.getDecoder().decode(CLEARPIXEL));
      bbjImageControl = w.addImageCtrl(img);
      ctrl = bbjImageControl;
      App.consoleLog("image added " + bbjImageControl);
      catchUp();
    } catch (Exception e) {
      App.consoleLog("Error: " + e.getMessage());
      Environment.logError(e);
    }
  }

  public Image getImage() {
    try {
      return (Image) bbjImageControl.getImage();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  public boolean isDisableable() {
    return bbjImageControl.isDisableable();
  }

  public void setDisableable(boolean disableable) {
    bbjImageControl.setDisableable(disableable);
  }

  public void setImage(Image image) {
    try {
      bbjImageControl.setImage((BBjImage) image);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setImageFromResource(String resourcePath) {
    byte[] data;
    try {
      data = this.getClass().getClassLoader().getResourceAsStream(resourcePath).readAllBytes();
    } catch (IOException e) {
      Environment.logError(e);
      return;
    }
    try {
      BBjImage img =
          Environment.getInstance().getSysGui().getImageManager().loadImageFromBytes(data);
      bbjImageControl.setImage(img);
    } catch (BBjException e) {
      Environment.logError(e);
    }

  }

  @Override
  public ImageControl setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public ImageControl setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public ImageControl setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public ImageControl setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public ImageControl setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public ImageControl addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public ImageControl removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }
}
