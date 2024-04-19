package com.webforj.data.binding;

import com.webforj.data.transformation.transformer.Transformer;

class StringIntegerTransformer implements Transformer<String, Integer> {

  @Override
  public Integer transformToModel(String viewValue) {
    return Integer.parseInt(viewValue);
  }

  @Override
  public String transformToComponent(Integer modelValue) {
    return modelValue.toString();
  }
}
