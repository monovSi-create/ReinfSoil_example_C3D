Imports DBTransactionManager = Autodesk.AutoCAD.DatabaseServices.TransactionManager
Imports OpenMode = Autodesk.AutoCAD.DatabaseServices.OpenMode
Public Class CladdingBlocks
    Inherits SATemplate
    Private Const dBlockWidth = 0.2
    Private Const dBlocksInLayout = 5
    Private Const dBlockHeight = 0.15
    Private Const deltaH = 0.000
    Private Const dBlockOffset = 0.00
    Private Const dBlockLength = 0.4
    Private Const SideDefault = Utilities.Right

    Private Shared _blocksCount As Integer = 0 'необходима для хранения значения на протяжении перестроения всего коридора

    Protected Overrides Sub GetInputParametersImplement(corridorState As CorridorState)
        MyBase.GetInputParametersImplement(corridorState)

        ' define collection for long parameters in corridor
        Dim paramsLong As ParamLongCollection
        paramsLong = corridorState.ParamsLong

        ' define collection for double parameters in corridor
        Dim paramsDouble As ParamDoubleCollection
        paramsDouble = corridorState.ParamsDouble

        Dim paramsString As ParamStringCollection
        paramsString = corridorState.ParamsString

        Dim oParam As IParam
        ' Add input parameters we used in this script
        oParam = paramsLong.Add(Utilities.Side, SideDefault)
        oParam = paramsLong.Add("BlocksInLayout", dBlocksInLayout)
        oParam = paramsDouble.Add("BlocksDeltaH", deltaH)
        oParam = paramsDouble.Add("BlockLength", dBlockLength)
        oParam = paramsDouble.Add("BlockHeight", dBlockHeight)
        oParam = paramsDouble.Add("BlockWidth", dBlockWidth)
        oParam = paramsDouble.Add("BlockOffset", dBlockOffset)
    End Sub
    Protected Overrides Sub GetLogicalNamesImplement(corridorState As CorridorState)
        MyBase.GetLogicalNamesImplement(corridorState)

        'retrieve paramater buckets from the corridor state
        Dim paramsLong As ParamLongCollection
        paramsLong = corridorState.ParamsLong
        'add logical names we used to script
        Dim ParamLong As ParamLong

        ParamLong = paramsLong.Add("designProfile", ParamLogicalNameType.ElevationTarget)
        ParamLong.DisplayName = "Проектный профиль"
        ParamLong = paramsLong.Add("blocksTop", ParamLogicalNameType.ElevationTarget)
        ParamLong.DisplayName = "Профиль облицовочных блоков"
    End Sub
    Protected Overrides Sub GetOutputParametersImplement(corridorState As CorridorState)
        MyBase.GetOutputParametersImplement(corridorState)

        ' Регистрируем выходной параметр BlocksCount
        Dim paramsLong As ParamLongCollection = corridorState.ParamsLong
        Dim paramsDouble As ParamDoubleCollection = corridorState.ParamsDouble
        Dim oParam As IParam

        oParam = paramsLong.Add("BlocksCount", _blocksCount)
        If oParam IsNot Nothing Then oParam.Access = ParamAccessType.Output

    End Sub

    Protected Overrides Sub DrawImplement(corridorState As CorridorState)
        Dim tm As DBTransactionManager
        tm = Autodesk.AutoCAD.DatabaseServices.HostApplicationServices.WorkingDatabase.TransactionManager

        Dim oParamsElevationTarget As ParamElevationTargetCollection
        oParamsElevationTarget = corridorState.ParamsElevationTarget

        Dim paramsString As ParamStringCollection
        paramsString = corridorState.ParamsString

        ' Retrieve parameter buckets from the corridor state
        Dim paramsLong As ParamLongCollection
        paramsLong = corridorState.ParamsLong

        Dim paramsDouble As ParamDoubleCollection
        paramsDouble = corridorState.ParamsDouble
        '-----------------------------------------
        ' on error resume next
#Region "переменные"
        Dim side As Long
        Try
            side = paramsLong.Value(Utilities.Side)
        Catch
            side = SideDefault
        End Try
        '----------------------------------------
        'flip about Y axis
        Dim flip As Double
        flip = 1.0#
        If side = Utilities.Left Then
            flip = -1.0#
        End If
        '----------------------------------------
        Dim blockLayers As Long
        Try
            blockLayers = paramsLong.Value("BlocksInLayout")
        Catch
            blockLayers = dBlocksInLayout
        End Try
        '----------------------------------------
        Dim dH As Double
        Try
            dH = paramsDouble.Value("BlocksDeltaH")
        Catch
            dH = deltaH
        End Try
        '----------------------------------------
        Dim dL As Double
        Try
            dL = paramsDouble.Value("BlockLength")
        Catch
            dL = dBlockLength
        End Try
        '----------------------------------------
        Dim bHeight As Double
        Try
            bHeight = paramsDouble.Value("BlockHeight")
        Catch
            bHeight = dBlockHeight
        End Try
        '----------------------------------------
        Dim bWidth As Double
        Try
            bWidth = paramsDouble.Value("BlockWidth")
        Catch
            bWidth = dBlockWidth
        End Try
        '----------------------------------------
        Dim bOffset As Double
        Try
            bOffset = paramsDouble.Value("BlockOffset")
        Catch
            bOffset = dBlockOffset
        End Try
#End Region
        Dim oOrigin As New PointInMem
        Dim oCurrentAlignmentId As ObjectId
        Utilities.GetAlignmentAndOrigin(corridorState, oCurrentAlignmentId, oOrigin)

        If corridorState.Mode <> CorridorMode.Layout Then
            '------------------------
            'проводим анализ сечения
            '------------------------
            'определяем профиль для вычисления высоты стены
            Dim elevationTarget As SlopeElevationTarget
            Try
                elevationTarget = oParamsElevationTarget.Value("designProfile")
            Catch
                elevationTarget = Nothing
            End Try
            Dim hasWallHeightProfile As Boolean
            hasWallHeightProfile = False
            Dim dWallHeight As Double

            If Not elevationTarget Is Nothing Then
                'получим высоту по профилю
                Try
                    dWallHeight = elevationTarget.GetElevation(oCurrentAlignmentId, corridorState.CurrentStation) - oOrigin.Elevation
                    hasWallHeightProfile = True
                Catch
                    Utilities.RecordWarning(corridorState, CorridorError.LogicalNameNotFound, "designProfile", "RetainWallVertical")
                End Try
            End If
            'определяем профиль облицовочных блоков (если имеется)
            Dim blocksElevTarget As SlopeElevationTarget
            Try
                blocksElevTarget = oParamsElevationTarget.Value("blocksTop")
            Catch
                blocksElevTarget = Nothing
            End Try

            Dim hasWallBlocksProfile As Boolean
            hasWallBlocksProfile = False
            Dim blocksHeight As Double
            Dim newOrigin As New PointInMem With {
                .Offset = 0,
                .Elevation = 0
                }
            Dim blockStep = bHeight + dH
            Dim rows As Integer
            Dim levelWidth = 0.2

            If Not blocksElevTarget Is Nothing Then
                'получим высоту по профилю
                Try
                    blocksHeight = blocksElevTarget.GetElevation(oCurrentAlignmentId, corridorState.CurrentStation) - oOrigin.Elevation
                    hasWallBlocksProfile = True
                Catch
                    Utilities.RecordWarning(corridorState, CorridorError.LogicalNameNotFound, "designProfile", "RetainWallVertical")
                End Try
                'сечение по заданному профилю высоты блоков+проектному
                rows = CType(blocksHeight \ blockStep, Integer)

            Else
                'в начале каждого региона(области)
                If corridorState.CurrentStation = corridorState.CurrentRegionStartStation Then
                    'создаем доп.сечения
                    createAddStations(tm, corridorState, blockStep, dL, elevationTarget)
                    ' Рассчитываем новое количество блоков на основе высоты
                    Dim divisor = blockStep * 1000
                    _blocksCount = dWallHeight * 1000 \ divisor
                    'доп условие: если стена опускается с самого начала
                    Dim firstTop As Double = 0
                    While firstTop <= dL / 2
                        If isStep(tm, corridorState, firstTop) Then
                            _blocksCount -= 1
                        End If
                        firstTop += 0.001
                    End While
                End If

                'условие для переопределения высоты облицовки
                If isStep(tm, corridorState, corridorState.CurrentStation) And corridorState.CurrentStation <> corridorState.CurrentRegionStartStation And corridorState.CurrentStation <> corridorState.CurrentRegionStartStation + 0.001 Then
                    'вспомогательные вектора до и после скачка для оценки направления проектного профиля
                    Dim beforeStep = elevationTarget.GetElevation(oCurrentAlignmentId, corridorState.CurrentStation - 0.01)
                    Dim afterStep = elevationTarget.GetElevation(oCurrentAlignmentId, corridorState.CurrentStation + 0.01)
                    'сравниваем текущую высоту по блокам и высоту луча(общую высоту стенки)
                    If beforeStep < afterStep Then
                        _blocksCount += 1
                    ElseIf beforeStep > afterStep Then
                        _blocksCount -= 1
                    Else
                        Throw New Exception("что-то неладное")
                    End If

                End If

                'создание выравнивающего слоя
                rows = _blocksCount
                paramsLong.Item("BlocksCount").Value = rows
                rows = paramsLong.Item("BlocksCount").Value

                'создание облицовочных блоков
            End If
            createCladding(corridorState, dWallHeight, flip, bHeight, bWidth, bOffset, rows, dH, levelWidth, newOrigin)

        Else 'for layout mode
            '----------------------------------
            'строим шаблон конструкции
            '----------------------------------
            Dim levelWidth = 0.2
            Dim levelH = 0.1
            Dim dWallHeight = blockLayers * (bHeight + dH) + levelH

            'создание облицовочных блоков
            createCladding(corridorState, dWallHeight, flip, bHeight, bWidth, bOffset, blockLayers, dH, levelWidth, oOrigin)
        End If
        ' Обновляем входные параметры (если требуется)
        paramsLong.Add(Utilities.Side, side)
        paramsLong.Add("BlocksInLayout", blockLayers)
        paramsDouble.Add("BlocksDeltaH", dH)
        paramsDouble.Add("BlockLength", dL)
        paramsDouble.Add("BlockHeight", bHeight)
        paramsDouble.Add("BlockWidth", bWidth)
        paramsDouble.Add("BlockOffset", bOffset)
    End Sub
    'создание конструкции
    Public Sub createCladding(ByVal corridorState As CorridorState,
                              ByVal dWallHeight As Double,
                              ByVal flipValue As Double,
                              ByVal block_H As Double,
                              ByVal block_W As Double,
                              ByVal block_Offset As Double,
                              ByVal blockRows As Integer,
                              ByVal delHeight As Double,
                              ByVal levelingWidth As Double,
                              ByVal originSub As PointInMem)

        Dim paramsLong As ParamLongCollection
        paramsLong = corridorState.ParamsLong

        Dim paramsDouble As ParamDoubleCollection
        paramsDouble = corridorState.ParamsDouble

        'создание облицовочных блоков
        Dim tanL = (block_Offset * 1000) / ((block_H + delHeight) * 1000)
        Dim totalOffset = dWallHeight * tanL

        'точки вставки облицовочных блоков
        Dim newAddPoint As New PointInMem With {
        .Offset = originSub.Offset - totalOffset * flipValue,
        .Elevation = originSub.Elevation
        }
        'точка вставки омоноличивания
        Dim levelingTopPoint As New PointInMem With {
        .Elevation = originSub.Elevation + dWallHeight,
        .Offset = 0
        }
        Dim i As Integer = 1
        While i <= blockRows
            createBlock(corridorState, newAddPoint, block_H, block_W, flipValue)
            newAddPoint.Offset += block_Offset * flipValue
            newAddPoint.Elevation += (block_H + delHeight)
            i += 1
        End While
        newAddPoint.Offset = 0
        levelingTop(corridorState, levelingTopPoint, newAddPoint, levelingWidth, flipValue)

        Dim oParam As IParam
        oParam = paramsLong.Add("BlocksCount", blockRows)
        If oParam IsNot Nothing Then
            oParam.Access = ParamAccessType.Output
        End If
    End Sub
    'создание облицовочного блока 
    Public Sub createBlock(corridorState As CorridorState, addPoint As PointInMem, height As Double, width As Double, flip As Double)
        '--------------
        Dim blockPoints As PointCollection
        blockPoints = corridorState.Points

        Dim blockLinks As LinkCollection
        blockLinks = corridorState.Links

        Dim blockShapes As ShapeCollection
        blockShapes = corridorState.Shapes

        Dim P1 As Point
        Dim P2 As Point
        Dim P3 As Point
        Dim P4 As Point
        Dim P5 As Point

        Dim L1 As Link
        Dim L2 As Link
        Dim L3 As Link
        Dim L4 As Link

        Dim Shape As Autodesk.Civil.DatabaseServices.Shape
        '-------------------------------------------------
        P1 = blockPoints.Add(addPoint.Offset, addPoint.Elevation, "")
        P2 = blockPoints.Add(P1.Offset, P1.Elevation + height, "")
        P3 = blockPoints.Add(P2.Offset + flip * width, P2.Elevation, "")
        P4 = blockPoints.Add(P3.Offset, P1.Elevation, "")

        P5 = blockPoints.Add(P1.Offset + flip * width / 2, P1.Elevation, "Blocks_Axis")

        L1 = blockLinks.Add(P1, P2, "Front_Surface")
        L2 = blockLinks.Add(P2, P3, "")
        L3 = blockLinks.Add(P3, P4, "")
        L4 = blockLinks.Add(P4, P1, "")

        Dim shapeLinks() = {L1, L2, L3, L4}
        Shape = blockShapes.Add(shapeLinks, "Cladding_block")

    End Sub
    'дополнительные сечения в точках скачка выравнивающего слоя (addition stations)
    Public Sub createAddStations(tm As DBTransactionManager, corridorState As CorridorState, blockStep As Double, blockLength As Double, target As SlopeElevationTarget)
        Dim origin As New PointInMem
        Dim alignmentId As ObjectId
        Utilities.GetAlignmentAndOrigin(corridorState, alignmentId, origin)
        'пробегаем по всей области и находим пикеты "скачка" блоков (run by region and find stations for steps of blocks)
        Dim startSt = corridorState.CurrentRegionStartStation
        Dim stateStep As Double = 0.001
        Dim endSt = corridorState.CurrentRegionEndStation
        Dim stationCurr = startSt + blockLength / 2
        Dim sectionsToAdd As New List(Of Double)
        Dim sectionsToAddStep As New List(Of Double)
        Dim sliseStep = blockLength / 2
        Do While stationCurr < endSt
            Dim wallHeight = target.GetElevation(alignmentId, stationCurr) - origin.Elevation
            Dim remainder = wallHeight Mod blockStep
            If Math.Abs(remainder) < 0.001 Then
                Dim rem1 = stationCurr Mod sliseStep
                Dim backSlice = stationCurr - rem1
                Dim rem2 = sliseStep - rem1
                Dim frontSlice = stationCurr + rem2
                Dim backH = target.GetElevation(alignmentId, backSlice)
                Dim frontH = target.GetElevation(alignmentId, frontSlice)
                If frontH <= backH Then
                    sectionsToAdd.Add(backSlice)
                    sectionsToAddStep.Add(backSlice + 0.001)
                Else
                    sectionsToAdd.Add(frontSlice)
                    sectionsToAddStep.Add(frontSlice + 0.001)
                End If
                stationCurr += sliseStep
            End If
            stationCurr += stateStep
        Loop

        Dim corridor As Corridor
        corridor = tm.GetObject(corridorState.CurrentCorridorId, OpenMode.ForWrite)
        Dim baselines As BaselineCollection
        baselines = corridor.Baselines
        Dim baseline As Baseline
        For Each b As Baseline In baselines
            If corridorState.CurrentProfileId = b.ProfileId Then
                baseline = b
                Dim regs As BaselineRegionCollection
                regs = baseline.BaselineRegions
                For Each reg As BaselineRegion In regs
                    If reg.StartStation = corridorState.CurrentRegionStartStation Or reg.EndStation = corridorState.CurrentRegionEndStation Then
                        'очищаем дополнительные сечения (delete add sections with names)
                        Dim settings = reg.AppliedAssemblySetting
                        Dim infos = settings.AdditionalAppliedAssemblies
                        For Each info In infos
                            Dim description1 = "доп.сечения облицовочных блоков " + baseline.Name
                            If info.Description = description1 Then
                                reg.DeleteStation(info.Station)
                            End If
                            Dim description2 = "скачок облицовки " + baseline.Name
                            If info.Description = description2 Then
                                reg.DeleteStation(info.Station)
                            End If
                        Next
                        'добавляем новые сечения (create add sections with names)
                        Dim assemblyStations As Double()
                        assemblyStations = reg.AppliedAssemblies.Stations
                        'если в точке нет сечения - создаем дополнительное
                        Dim diff1 = sectionsToAdd.Except(assemblyStations)
                        Dim diff2 = sectionsToAddStep.Except(assemblyStations)
                        For Each station In diff1
                            Try
                                reg.AddStation(station, "доп.сечения облицовочных блоков " + baseline.Name)
                            Catch

                            End Try
                        Next
                        For Each station In diff2
                            Try
                                reg.AddStation(station, "скачок облицовки " + baseline.Name)
                            Catch

                            End Try
                        Next
                    End If
                Next
            End If
        Next
    End Sub
    'условие для пересчета высоты облицовки (создание ступени) (condition for creation gap)
    Public Function isStep(tm As DBTransactionManager, corridorState As CorridorState, stationCurr As Double)
        Dim result As Boolean = False
        Dim corridor As Corridor
        corridor = tm.GetObject(corridorState.CurrentCorridorId, OpenMode.ForWrite)
        Dim baselines As BaselineCollection
        baselines = corridor.Baselines
        Dim baseline As Baseline
        For Each b As Baseline In baselines
            If corridorState.CurrentProfileId = b.ProfileId Then
                baseline = b
                Dim regs As BaselineRegionCollection
                regs = baseline.BaselineRegions
                For Each reg As BaselineRegion In regs
                    If reg.StartStation = corridorState.CurrentRegionStartStation Or reg.EndStation = corridorState.CurrentRegionEndStation Then
                        'получаем свойства доп сечений
                        Dim settings = reg.AppliedAssemblySetting
                        Dim infos = settings.AdditionalAppliedAssemblies
                        For Each info In infos
                            Dim description = "скачок облицовки " + baseline.Name
                            If info.Description = description And stationCurr = info.Station Then
                                result = True
                            End If
                        Next
                    End If
                Next
            End If
        Next
        Return result
    End Function
    'метод для создания выравнивающей ленты 
    Public Sub levelingTop(ByVal corridorState As CorridorState,
                           ByVal topPoint As PointInMem,
                           ByVal lowPoint As PointInMem,
                           ByVal Width As Double,
                           ByVal flip As Double)
        Dim levelPoints As PointCollection
        levelPoints = corridorState.Points

        Dim levelLinks As LinkCollection
        levelLinks = corridorState.Links

        Dim levelShapes As ShapeCollection
        levelShapes = corridorState.Shapes

        Dim oLevelP1 As Point
        Dim oLevelP2 As Point
        Dim oLevelP3 As Point
        Dim oLevelP4 As Point

        Dim oLevelL1 As Link
        Dim oLevelL2 As Link
        Dim oLevelL3 As Link
        Dim oLevelL4 As Link

        Dim oLevelShape As Autodesk.Civil.DatabaseServices.Shape
        If topPoint.Elevation < lowPoint.Elevation Then
            topPoint.Elevation = lowPoint.Elevation
        End If
        oLevelP1 = levelPoints.Add(lowPoint.Offset, lowPoint.Elevation, "Низ выравнивающего слоя")
        oLevelP2 = levelPoints.Add(topPoint.Offset, topPoint.Elevation, "Верх выравнивающего слоя")
        oLevelP3 = levelPoints.Add(oLevelP2.Offset + Width * flip, oLevelP2.Elevation, "")
        oLevelP4 = levelPoints.Add(oLevelP1.Offset + Width * flip, oLevelP1.Elevation, "")

        oLevelL1 = levelLinks.Add(oLevelP1, oLevelP2, "")
        oLevelL2 = levelLinks.Add(oLevelP2, oLevelP3, "")
        oLevelL3 = levelLinks.Add(oLevelP3, oLevelP4, "")
        oLevelL4 = levelLinks.Add(oLevelP4, oLevelP1, "")

        oLevelShape = levelShapes.Add(oLevelL1, oLevelL2, oLevelL3, oLevelL4, "Выравнивающий слой под МШБ")
    End Sub
End Class
