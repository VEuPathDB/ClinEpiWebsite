package org.clinepi.service.accessRequest;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Map;

import org.apache.log4j.Logger;
import org.eupathdb.common.model.contact.EmailSender;
import org.gusdb.fgputil.db.SqlUtils;
import org.gusdb.wdk.model.WdkModel;
import org.gusdb.wdk.model.WdkModelException;
import org.gusdb.wdk.model.config.ModelConfig;

import static org.gusdb.fgputil.FormatUtil.escapeHtml;

public class AccessRequestSubmitter {
  private static final Logger LOG = Logger.getLogger(AccessRequestSubmitter.class);

  public enum SubmissionResult {
    SUCCESSFUL,
    ALREADY_REQUESTED
  }

  public static SubmissionResult submitAccessRequest(AccessRequestParams params, WdkModel wdkModel, EmailSender emailSender) throws SQLException, WdkModelException {
    boolean requestInitiated = false;

    // In one transaction...
    //   (1) insert a DB record for the new request and
    //   (2) email the request to the appropriate parties
    try (
        Connection conn = wdkModel.getAccountDb().getDataSource().getConnection();
    ) {
      conn.setAutoCommit(false);
      String sql = insertRequestPreparedStatementBody();

      try (
          PreparedStatement ps = insertRequestPreparedStatement(conn, sql, params);
      ) {
        SqlUtils.executePreparedStatement(ps, sql, "write-access-request");
        boolean insertionPerformed = ps.getUpdateCount() == 1;

        if (insertionPerformed) {
          if (params.approvalNeeded() && !params.inTestMode()) {
            emailAccessRequest(emailSender, params, wdkModel);
          }

          requestInitiated = true;
        }

        if (!params.inTestMode()) {
          conn.commit();
        }
      }
      // Either the DB update (SQLException) or email submission (WdkModelException)
      // has failed, and so we roll back the record insertion
      catch (SQLException | WdkModelException ex) {
        conn.rollback();
        throw new WdkModelException(ex);
      }
    }

    return requestInitiated || params.inTestMode() ? SubmissionResult.SUCCESSFUL : SubmissionResult.ALREADY_REQUESTED;
  }

  private static String insertRequestPreparedStatementBody() {
    return "INSERT INTO\n"
      + "  studyaccess.end_users (\n"
      + "    user_id\n"
      + "  , dataset_presenter_id\n"
      + "  , purpose\n"
      + "  , research_question\n"
      + "  , analysis_plan\n"
      + "  , dissemination_plan\n"
      + "  , prior_auth\n"
      + "  , restriction_level_id\n"
      + "  , approval_status_id\n"
      + "  )\n"
      + "SELECT\n"
      + "  ? -- user_id\n"
      + ", ? -- dataset_presenter_id\n"
      + ", ? -- purpose\n"
      + ", ? -- research_question\n"
      + ", ? -- analysis_plan\n"
      + ", ? -- dissemination_plan\n"
      + ", ? -- prior_auth\n"
      + ", (\n"
      + "    SELECT restriction_level_id\n"
      + "    FROM studyaccess.restriction_level"
      + "    WHERE name = ?"
      + "  ) -- restriction_level\n"
      + ", ? -- approval_status\n"
      + "FROM dual\n"
      + "WHERE NOT EXISTS (\n"
      + "  SELECT user_id, dataset_presenter_id\n"
      + "  FROM studyaccess.end_users\n"
      + "  WHERE user_id = ?\n"
      + "    AND dataset_presenter_id = ?\n"
      + ")";
  }

  private static PreparedStatement insertRequestPreparedStatement(Connection conn, String psBody, AccessRequestParams params)
      throws SQLException {
    PreparedStatement ps = conn.prepareStatement(psBody);

    ps.setInt(1, params.getUserId());
    ps.setString(2, params.getDatasetId());
    ps.setString(3, params.getPurpose());
    ps.setString(4, params.getResearchQuestion());
    ps.setString(5, params.getAnalysisPlan());
    ps.setString(6, params.getDisseminationPlan());
    ps.setString(7, params.getPriorAuth());
    ps.setString(8, params.getRestrictionLevel());
    ps.setInt(9, params.getApprovalType());
    ps.setInt(10, params.getUserId());
    ps.setString(11, params.getDatasetId());

    return ps;
  }

  public static void emailAccessRequest(EmailSender emailSender, AccessRequestParams params, WdkModel wdkModel) throws WdkModelException {
    ModelConfig modelConfig = wdkModel.getModelConfig();
    String version = wdkModel.getBuildNumber();
    String website = wdkModel.getDisplayName();
    String supportEmail = wdkModel.getProperties().get("CLINEPI_ACCESS_REQUEST_EMAIL");
    String requesterEmail = params.getRequesterEmail();
    String datasetName = params.getDatasetName();

    LOG.debug("emailAccessRequest() -- requesterEmail: " + requesterEmail);
    LOG.debug("emailAccessRequest() -- providerEmail: not needed" + params.getProviderEmail());

    String bodyTemplate = params.getBodyTemplate();
    Map<String, String> formFields = params.getFormFields();
    String subject = String.format(
      "%s (%s) Requests Access to ClinEpiDB Dataset %s",
      params.getRequesterName(),
      requesterEmail,
      datasetName
    );
    LOG.debug("emailAccessRequest() -- here are the formFields: " + formFields);
    String body = createAccessRequestEmailBody(bodyTemplate, formFields, datasetName);

    String replyEmail = requesterEmail;
    String metaInfo =
        "ReplyTo: " + replyEmail + "\n" +
        "WDK Model version: " + version;

    String redmineContent = "****THIS IS NOT A REPLY****" +
                            "This is an automatic response, that includes your message for your records, to let you " +
                            "know that we have received your email and will get back to you as " +
                            "soon as possible. Thanks so much for contacting us!" +
                            "This was your message:" + "\n\n" + body + "\n";

    String redmineMetaInfo = "Project: clinepidb\n" + "Category: " +
        website + "\n" + "\n" +
        metaInfo + "\n";
    String smtpServer = modelConfig.getSmtpServer();

    // Send auto-reply
    emailSender.sendEmail(
      smtpServer,
      replyEmail,    //to
      supportEmail,  //reply (from)
      subject,
      escapeHtml(metaInfo) + "\n\n" + redmineContent + "\n\n",
      null,null,
      null
    );

    // Send support email (help@)
    emailSender.sendEmail(
      smtpServer,
      supportEmail, //or params.getProviderEmail(),
      requesterEmail,
      subject,
      body,
      null, //not needed, already sent to support
      params.getBccEmail(),
      null
    );

    // Send Redmine email
    emailSender.sendEmail(
      smtpServer,
      wdkModel.getProperties().get("REDMINE_TO_EMAIL"),   //sendTos
      wdkModel.getProperties().get("REDMINE_FROM_EMAIL"), //reply
      subject,
      escapeHtml(redmineMetaInfo) + "\n\n" + body + "\n\n",
      null,null,null
    );

  }

  private static String createAccessRequestEmailBody(String bodyTemplate, Map<String, String> formFields, String datasetName) {
    String bodyWithFilledOutFormFields = formFields.entrySet().stream().reduce(
      bodyTemplate,
      (body, entry) -> body.replaceAll(
        "\\$\\$" + entry.getKey().toUpperCase() + "\\$\\$",
        escapeHtml(entry.getValue())
      ),
      String::concat
    );

    return bodyWithFilledOutFormFields.replaceAll(
      "\\$\\$DATASET_NAME\\$\\$",
      escapeHtml(datasetName)
    );
  }
}
