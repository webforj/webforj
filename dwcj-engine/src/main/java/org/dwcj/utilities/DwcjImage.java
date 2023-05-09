package org.dwcj.utilities;

import java.io.File;
import java.io.FileOutputStream;

public class DwcjImage {
  // private File file;

  // DwcjImage(String path) {
  // this.file = new File(path);
  // }

  // DwcjImage(File file) {
  // this.file = file;
  // }

  public static DwcjImage loadImageFromBytes(byte[] bytes) {
    ImageUtil.convertBytestoBBjImage(bytes);
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromClientJar(String filename) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromClientUrl(String url) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromFile(String filename) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromFile(String filename, int index) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromImageList(int id, int index) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromServerJar(String filename) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromServerUrl(String url) {
    return new DwcjImage();
  }

  public static DwcjImage loadImageFromUrl(String url) {
    return new DwcjImage();
  }

}
