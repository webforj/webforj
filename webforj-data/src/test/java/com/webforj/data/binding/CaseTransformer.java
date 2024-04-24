package com.webforj.data.binding;

import com.webforj.data.transformation.transformer.Transformer;

class CaseTransformer implements Transformer<String, String> {

  @Override
  public String transformToModel(String viewValue) {
    return viewValue.toUpperCase();
  }

  @Override
  public String transformToComponent(String modelValue) {
    return modelValue.toLowerCase();
  }
}
