package com.webforj.data.binding;

import com.webforj.data.ZoneAware;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.Validator;
import java.time.ZoneId;

class ZoneValidator implements Validator<String>, ZoneAware<ZoneValidator> {
  private ZoneId zone;

  @Override
  public ValidationResult validate(String value) {
    return ValidationResult.valid();
  }

  @Override
  public ZoneValidator setZone(ZoneId zone) {
    this.zone = zone;
    return this;
  }

  @Override
  public ZoneId getZone() {
    return zone;
  }
}
