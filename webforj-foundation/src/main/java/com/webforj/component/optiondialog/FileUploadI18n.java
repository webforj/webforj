package com.webforj.component.optiondialog;

import com.google.gson.Gson;
import com.google.gson.annotations.SerializedName;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * The file upload translation object.
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public class FileUploadI18n {

  @SerializedName("approve")
  private String upload = "Upload";
  private String cancel = "Cancel";
  private String dropFile = "Drop file to upload";
  private String dropFiles = "Drop files to upload";
  private String addFile = "Select File...";
  private String addFiles = "Select Files...";
  private String filterAll = "All Files";
  private String uploadFile = "Upload";
  private String clearFile = "Clear";
  private String retryFile = "Retry upload";
  private String statusMaxed = "The max amount of allowed files has been reached";
  private String statusTooBig = "File is too big.";
  private String statusNotAllowedType = "File type is not allowed.";
  private String statusConnecting = "Connecting...";
  private String statusQueued = "Queued";
  private String statusCanceled = "Canceled";
  private String errorUnavailable = "Server Unavailable";
  private String errorUnexpected = "Unexpected Server Error";
  private String errorForbidden = "Forbidden";

  /**
   * Gets the approve message.
   *
   * @return the approve message.
   */
  public String getUpload() {
    return upload;
  }

  /**
   * Sets the approve message.
   *
   * @param upload the approve message.
   */
  public void setUpload(String upload) {
    this.upload = upload;
  }

  /**
   * Gets the cancel message.
   *
   * @return the cancel message.
   */
  public String getCancel() {
    return cancel;
  }

  /**
   * Sets the cancel message.
   *
   * @param cancel the cancel message.
   */
  public void setCancel(String cancel) {
    this.cancel = cancel;
  }

  /**
   * Gets the drop file message.
   *
   * @return the drop file message.
   */
  public String getDropFile() {
    return dropFile;
  }

  /**
   * Sets the drop file message.
   *
   * @param dropFile the drop file message.
   */
  public void setDropFile(String dropFile) {
    this.dropFile = dropFile;
  }

  /**
   * Gets the drop files message.
   *
   * @return the drop files message.
   */
  public String getDropFiles() {
    return dropFiles;
  }

  /**
   * Sets the drop files message.
   *
   * @param dropFiles the drop files message.
   */
  public void setDropFiles(String dropFiles) {
    this.dropFiles = dropFiles;
  }

  /**
   * Gets the add file message.
   *
   * @return the add file message.
   */
  public String getAddFile() {
    return addFile;
  }

  /**
   * Sets the add file message.
   *
   * @param addFile the add file message.
   */
  public void setAddFile(String addFile) {
    this.addFile = addFile;
  }

  /**
   * Gets the add files message.
   *
   * @return the add files message.
   */
  public String getAddFiles() {
    return addFiles;
  }

  /**
   * Sets the add files message.
   *
   * @param addFiles the add files message.
   */
  public void setAddFiles(String addFiles) {
    this.addFiles = addFiles;
  }

  /**
   * Gets the filter all message.
   *
   * @return the filter all message.
   */
  public String getFilterAll() {
    return filterAll;
  }

  /**
   * Sets the filter all message.
   *
   * @param filterAll the filter all message.
   */
  public void setFilterAll(String filterAll) {
    this.filterAll = filterAll;
  }

  /**
   * Gets the upload file message.
   *
   * @return the upload file message.
   */
  public String getUploadFile() {
    return uploadFile;
  }

  /**
   * Sets the upload file message.
   *
   * @param uploadFile the upload file message.
   */
  public void setUploadFile(String uploadFile) {
    this.uploadFile = uploadFile;
  }

  /**
   * Gets the clear file message.
   *
   * @return the clear file message.
   */
  public String getClearFile() {
    return clearFile;
  }

  /**
   * Sets the clear file message.
   *
   * @param clearFile the clear file message.
   */
  public void setClearFile(String clearFile) {
    this.clearFile = clearFile;
  }

  /**
   * Gets the retry file message.
   *
   * @return the retry file message.
   */
  public String getRetryFile() {
    return retryFile;
  }

  /**
   * Sets the retry file message.
   *
   * @param retryFile the retry file message.
   */
  public void setRetryFile(String retryFile) {
    this.retryFile = retryFile;
  }

  /**
   * Gets the status maxed message.
   *
   * @return the status maxed message.
   */
  public String getStatusMaxed() {
    return statusMaxed;
  }

  /**
   * Sets the status maxed message.
   *
   * @param statusMaxed the status maxed message.
   */
  public void setStatusMaxed(String statusMaxed) {
    this.statusMaxed = statusMaxed;
  }

  /**
   * Gets the status too big message.
   *
   * @return the status too big message.
   */
  public String getStatusTooBig() {
    return statusTooBig;
  }

  /**
   * Sets the status too big message.
   *
   * @param statusTooBig the status too big message.
   */
  public void setStatusTooBig(String statusTooBig) {
    this.statusTooBig = statusTooBig;
  }

  /**
   * Gets the status not allowed type message.
   *
   * @return the status not allowed type message.
   */
  public String getStatusNotAllowedType() {
    return statusNotAllowedType;
  }

  /**
   * Sets the status not allowed type message.
   *
   * @param statusNotAllowedType the status not allowed type message.
   */
  public void setStatusNotAllowedType(String statusNotAllowedType) {
    this.statusNotAllowedType = statusNotAllowedType;
  }

  /**
   * Gets the status connecting message.
   *
   * @return the status connecting message.
   */
  public String getStatusConnecting() {
    return statusConnecting;
  }

  /**
   * Sets the status connecting message.
   *
   * @param statusConnecting the status connecting message.
   */
  public void setStatusConnecting(String statusConnecting) {
    this.statusConnecting = statusConnecting;
  }

  /**
   * Gets the status queued message.
   *
   * @return the status queued message.
   */
  public String getStatusQueued() {
    return statusQueued;
  }

  /**
   * Sets the status queued message.
   *
   * @param statusQueued the status queued message.
   */
  public void setStatusQueued(String statusQueued) {
    this.statusQueued = statusQueued;
  }

  /**
   * Gets the status canceled message.
   *
   * @return the status canceled message.
   */
  public String getStatusCanceled() {
    return statusCanceled;
  }

  /**
   * Sets the status canceled message.
   *
   * @param statusCanceled the status canceled message.
   */
  public void setStatusCanceled(String statusCanceled) {
    this.statusCanceled = statusCanceled;
  }

  /**
   * Gets the error unavailable message.
   *
   * @return the error unavailable message.
   */
  public String getErrorUnavailable() {
    return errorUnavailable;
  }

  /**
   * Sets the error unavailable message.
   *
   * @param errorUnavailable the error unavailable message.
   */
  public void setErrorUnavailable(String errorUnavailable) {
    this.errorUnavailable = errorUnavailable;
  }

  /**
   * Gets the error unexpected message.
   *
   * @return the error unexpected message.
   */
  public String getErrorUnexpected() {
    return errorUnexpected;
  }

  /**
   * Sets the error unexpected message.
   *
   * @param errorUnexpected the error unexpected message.
   */
  public void setErrorUnexpected(String errorUnexpected) {
    this.errorUnexpected = errorUnexpected;
  }

  /**
   * Gets the error forbidden message.
   *
   * @return the error forbidden message.
   */
  public String getErrorForbidden() {
    return errorForbidden;
  }

  /**
   * Sets the error forbidden message.
   *
   * @param errorForbidden the error forbidden message.
   */
  public void setErrorForbidden(String errorForbidden) {
    this.errorForbidden = errorForbidden;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    byte[] bytes = new Gson().toJson(this).getBytes(StandardCharsets.UTF_8);
    bytes = Base64.getEncoder().encode(bytes);
    return new String(bytes, StandardCharsets.UTF_8);
  }
}
