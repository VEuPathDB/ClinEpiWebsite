package org.clinepi.service.services;

import static org.gusdb.fgputil.FormatUtil.NL;
import static org.gusdb.fgputil.FormatUtil.enumValuesAsString;
import static org.gusdb.fgputil.db.stream.ResultSetInputStream.getResultSetStream;
import static org.gusdb.fgputil.functional.Functions.executesWithoutException;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;
import org.gusdb.fgputil.Timer;
import org.gusdb.fgputil.db.ResultSetColumnInfo;
import org.gusdb.fgputil.db.runner.SQLRunner;
import org.gusdb.fgputil.db.stream.ResultSetInputStream.ResultSetRowConverter;
import org.gusdb.fgputil.db.stream.ResultSetToNdJsonConverter;
import org.gusdb.fgputil.functional.Functions;
import org.gusdb.fgputil.runtime.GusHome;
import org.gusdb.wdk.model.WdkModel;
import org.gusdb.wdk.model.WdkModelException;
import org.gusdb.wdk.service.service.AbstractWdkService;

@Path("/shiny")
public class ShinyQueryService extends AbstractWdkService {

  @SuppressWarnings("unused")
  private static final Logger LOG = Logger.getLogger(ShinyQueryService.class);

  private static final int FETCH_SIZE = 10000;

  private enum Mode {
    PrintSql,
    RunQuery;
  }

  private enum DataType {
    Participant,
    Household,
    Observation,
    ObservationNames,
    Sample;
  }

  private Response getStreamingResponse(String sql, String queryName, String errorMsgOnFail) throws WdkModelException {
    return Response.ok(
      getStreamingOutput(
        Functions.mapException(
          () -> getResultSetStream(sql, queryName,
              getWdkModel().getAppDb().getDataSource(),
              FETCH_SIZE, new ResultSetToNdJsonConverter()),
          e -> new WdkModelException(errorMsgOnFail + " SQL: " + sql, e)
        )
      )
    ).build();
  }

  public static void main(String[] args) throws WdkModelException, IOException {
    if (args.length != 7 ||
        !executesWithoutException(Mode::valueOf, args[0]) ||
        !executesWithoutException(DataType::valueOf, args[2])) {
      System.err.println(NL +
          "USAGE: fgpJava " + ShinyQueryService.class.getName() +
          " <mode> <projectId> <dataType> <tblPrefix> <sourceId> <timeSourceId> <outputFile>" + NL + NL +
          "Notes:" + NL +
          "  mode must be one of " + enumValuesAsString(Mode.values()) + NL +
          "  dataType must be one of " + enumValuesAsString(DataType.values()) + NL + NL);
      System.exit(1);
    }

    //String sql = getSql(DataType.Household, "OBI_0001627", "D2a6ace17a1", "EUPATH_0015467");
    String sql = getSql(DataType.valueOf(args[2]), args[3], args[4], args[5]);
    System.out.println("Using SQL: " + sql);

    if (Mode.valueOf(args[0]).equals(Mode.RunQuery)) {
      try (WdkModel wdkModel = WdkModel.construct(args[1], GusHome.getGusHome());
           BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(args[6]), 10240)) {
        Timer t = new Timer();
        ResultSetRowConverter rowConverter = new ResultSetToNdJsonConverter();
        new SQLRunner(wdkModel.getAppDb().getDataSource(), sql)
          .executeQuery(rs -> {
            try {
              ResultSetColumnInfo metadata = new ResultSetColumnInfo(rs);
              out.write(rowConverter.getHeader());
              if (rs.next()) {
                out.write(rowConverter.getRow(rs, metadata));
              }
              while (rs.next()) {
                out.write(rowConverter.getRowDelimiter());
                out.write(rowConverter.getRow(rs, metadata));
              }
              out.write(rowConverter.getFooter());
              return null;
            }
            catch (IOException e) {
              throw new RuntimeException(e);
            }
          });
        System.out.println("Wrote file in " + t.getElapsedString());
      }
    }
  }

  @GET
  @Path("Participant/{tblPrefix}/{sourceId}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response getShinyParticipantData(
      @PathParam("tblPrefix") String tblPrefix,
      @PathParam("sourceId") String sourceId,
      @DefaultValue("none") @QueryParam("timeSourceId") String timeSourceId)
          throws WdkModelException {
    return getStreamingResponse(getSql(DataType.Participant, tblPrefix, sourceId, timeSourceId),
        "getShinyParticipantData", "Failed running SQL to fetch participant data.");
  }

  @GET
  @Path("Household/{tblPrefix}/{sourceId}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response getShinyHouseholdData(
      @PathParam("tblPrefix") String tblPrefix,
      @PathParam("sourceId") String sourceId,
      @DefaultValue("none") @QueryParam("timeSourceId") String timeSourceId) 
          throws WdkModelException {
    return getStreamingResponse(getSql(DataType.Household, tblPrefix, sourceId, timeSourceId),
        "getShinyHouseholdData", "Failed running SQL to fetch household data.");
  }

  @GET
  @Path("Observation/{tblPrefix}/{sourceId}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response getShinyObservationData(
      @PathParam("tblPrefix") String tblPrefix,
      @PathParam("sourceId") String sourceId,
      @DefaultValue("none") @QueryParam("timeSourceId") String timeSourceId) 
          throws WdkModelException {
    return getStreamingResponse(getSql(DataType.Observation, tblPrefix, sourceId, timeSourceId),
        "getShinyObservationData", "Failed running SQL to fetch observation data.");
  }

  @GET
  @Path("ObservationNames/{tblPrefix}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response getShinyObservationNamesData(
      @PathParam("tblPrefix") String tblPrefix,
      @DefaultValue("none") @QueryParam("timeSourceId") String timeSourceId) 
          throws WdkModelException {
    return getStreamingResponse(getSql(DataType.ObservationNames, tblPrefix, null, timeSourceId),
        "getShinyObservationNamesData", "Failed running SQL to fetch observation name data.");
  }

  @GET
  @Path("Sample/{tblPrefix}/{sourceId}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response getShinySampleData(
      @PathParam("tblPrefix") String tblPrefix,
      @PathParam("sourceId") String sourceId,
      @DefaultValue("none") @QueryParam("timeSourceId") String timeSourceId)
          throws WdkModelException {
    return getStreamingResponse(getSql(DataType.Sample, tblPrefix, sourceId, timeSourceId),
        "getShinySampleData", "Failed running SQL to fetch sample data.");
  }

  private static String getSql(DataType dataType, String tblPrefix, String sourceId, String timeSourceId) {
    switch(dataType) {
      case Participant:
        return timeSourceId.equals("none")
            ? " select pa.name as Participant_Id" + 
                    ", pa." + sourceId +
              " from apidbtuning." + tblPrefix + "Participants pa" +
              " where pa." + sourceId + " is not null"
            : " select pa.name as Participant_Id" +
                    ", pa." + sourceId + 
                    ", oa." + timeSourceId +
              " from apidbtuning." + tblPrefix + "Participants pa" +
                  ", apidbtuning." + tblPrefix + "Observations oa" +
                  ", apidbtuning." + tblPrefix + "PartObsIO io" +
              " where pa.pan_id = io.participant_id" +
              " and io.observation_id = oa.pan_id" +
              " and pa." + sourceId + " is not null";
      case Household:
        return timeSourceId.equals("none")
            ? " select pa.name as Participant_Id" +
                    ", h.string_value as " + sourceId +
              " from apidbtuning." + tblPrefix + "HouseholdMD h" +
                  ", apidbtuning." + tblPrefix + "HousePartIO hp" +
                  ", apidbtuning." + tblPrefix + "Participants pa" +
              " where h.ontology_term_name = '" + sourceId + "'" +
              " and hp.household_id = h.household_id" +
              " and hp.participant_id = pa.pan_id"   

            :"with termtime as (" +
                " select h1.household_id" +
                      ", h1.string_value as " + sourceId +
                      ", h2.string_value as " + timeSourceId +
                " from apidbTuning." + tblPrefix + "HouseholdMD h1" +
                    ", apidbTuning." + tblPrefix + "HouseholdMD h2" +
                " where h1.ontology_term_name = '" + sourceId + "'" +
                " and h2.ontology_term_name = '" + timeSourceId + "'" +
                " and h1.household_id = h2.household_id" +
                " and exists (select ontology_term_name" +
                            " from apidbTuning." + tblPrefix + "HouseholdMD" +
                            " where ontology_term_name = '" + sourceId + "'" +
                            " and household_id = household_observation_id)" +
                " UNION ALL" +
                " select h1.household_id" +
                      ", h1.string_value as " + sourceId +
                      ", h2.string_value as " + timeSourceId +
                " from apidbTuning." + tblPrefix + "HouseholdMD h1" +
                    ", apidbTuning." + tblPrefix + "HouseholdMD h2" +
                " where h1.ontology_term_name = '" + sourceId + "'" +
                " and h2.ontology_term_name = '" + timeSourceId + "'" +
                " and h1.household_observation_id = h2.household_observation_id" +
                " and not exists (select ontology_term_name" +
                                " from apidbTuning." + tblPrefix + "HouseholdMD" +
                                " where ontology_term_name = '" + sourceId + "'" +
                                " and household_id = household_observation_id)" +
             ")" +
             " select p.name as participant_id" +
                  ", termtime." + sourceId +
                  ", termtime." + timeSourceId +
             " from termtime" +
                 ", apidbTuning." + tblPrefix + "HousePartIO hp" +
                 ", apidbTuning." + tblPrefix + "Participants p" +
             " where termtime.household_id = hp.household_id" +
             " and hp.participant_id = p.pan_id";
      case Observation:
        return timeSourceId.equals("none")
            ? " select pa.name as participant_id" +
                    ", string_value as " + sourceId +
              " from apidbtuning." + tblPrefix + "ObservationMD o" +
                  ", apidbtuning." + tblPrefix + "Participants pa" +
              " where o.ontology_term_name = '" + sourceId + "'" +
              " and o.participant_id = pa.pan_id"
            : " select pa.name as participant_id" +
                      ", o1.string_value as " + sourceId +
                      ", o2.string_value as " + timeSourceId +
                " from apidbtuning." + tblPrefix + "ObservationMD o1" +
                    ", apidbtuning." + tblPrefix + "ObservationMD o2" +
                    ", apidbtuning." + tblPrefix + "Participants pa" +
                " where o1.ontology_term_name = '" + sourceId + "'" +
                " and o2.ontology_term_name = '" + timeSourceId + "'" +
                " and o1.sub_observation_id = o2.sub_observation_id" +
                " and o1.participant_id = pa.pan_id";
      case ObservationNames:
        return timeSourceId.equals("none")
            ? " select pa.name as participant_id" +
                    ", oa.name as observation_id" +
              " from apidbtuning." + tblPrefix + "Observations oa" +
                  ", apidbtuning." + tblPrefix + "Participants pa" +
                  ", apidbtuning." + tblPrefix + "PartObsIO po" +  
              " where oa.pan_id = po.observation_id" +
              " and po.participant_id = pa.pan_id"
            : " select pa.name as participant_id" +
                    ", oa.name as observation_id" +
                    ", oa." + timeSourceId + 
              " from apidbtuning." + tblPrefix + "Observations oa" +
                  ", apidbtuning." + tblPrefix + "Participants pa" +
                  ", apidbtuning." + tblPrefix + "PartObsIO po" +
              " where oa.pan_id = po.observation_id" +
              " and po.participant_id = pa.pan_id";
      case Sample:
        return timeSourceId.equals("none")
            ? " select pa.name as Participant_Id, sa." + sourceId +
              " from apidbtuning." + tblPrefix + "Participants pa" +
                  ", apidbtuning." + tblPrefix + "Samples sa "+
                  ", apidbtuning." + tblPrefix + "PartObsIO io" +
                  ", apidbtuning." + tblPrefix + "ObsSampleIO io2" +
              " where pa.pan_id = io.participant_id" +
              " and io.observation_id = io2.observation_id" +
              " and io2.sample_id = sa.pan_id" +
              " and sa." + sourceId + " is not null"
            : " select pa.name as Participant_Id" +
                    ", sa." + sourceId +
                    ", oa." + timeSourceId +
              " from apidbtuning." + tblPrefix + "Participants pa" +
                  ", apidbtuning." + tblPrefix + "Observations oa" +
                  ", apidbtuning." + tblPrefix + "Samples sa "+
                  ", apidbtuning." + tblPrefix + "PartObsIO io" +
                  ", apidbtuning." + tblPrefix + "ObsSampleIO io2" +
              " where pa.pan_id = io.participant_id" +
              " and io.observation_id = oa.pan_id" +
              " and io.observation_id = io2.observation_id" +
              " and io2.sample_id = sa.pan_id" +
              " and sa." + sourceId + " is not null";
        default:
          throw new IllegalArgumentException("Unsupported data type: " + dataType);
    }
  }
}
