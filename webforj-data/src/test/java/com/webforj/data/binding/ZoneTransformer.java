package com.webforj.data.binding;

import com.webforj.data.ZoneAware;
import com.webforj.data.transformation.transformer.Transformer;
import java.time.ZoneId;

class ZoneTransformer implements Transformer<String, String>, ZoneAware<ZoneTransformer> {
  private ZoneId zone;

  @Override
  public String transformToModel(String viewValue) {
    return viewValue;
  }

  @Override
  public String transformToComponent(String modelValue) {
    return modelValue;
  }

  @Override
  public ZoneTransformer setZone(ZoneId zone) {
    this.zone = zone;
    return this;
  }

  @Override
  public ZoneId getZone() {
    return zone;
  }
}
