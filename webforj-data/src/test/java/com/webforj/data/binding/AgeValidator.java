package com.webforj.data.binding;

import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.Validator;

class AgeValidator implements Validator<Integer> {

  @Override
  public ValidationResult validate(Integer value) {
    // Example validation
    return value > 18 ? ValidationResult.valid() : ValidationResult.invalid("Age must be over 18");
  }
}
