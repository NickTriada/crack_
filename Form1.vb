Imports Emgu.CV
Imports Emgu.CV.Structure
Imports Emgu.Util
Imports System.Windows.Forms
Imports System.Drawing
Imports System.Runtime.InteropServices
Imports System.IO
Imports Emgu.CV.CvEnum
Imports System.Drawing.Imaging
Imports el_crack_dll_.crack


Public Class Form1
    Private mCurrentImage As Image
    Private mFolder As String
    Private mImageList As ArrayList
    Private mImagePosition As Integer

    Private Sub RUN_FROM_DLL_Click(sender As Object, e As EventArgs) Handles RUN_FROM_DLL.Click
        Dim result As DialogResult = OpenFileDialog1.ShowDialog()
        If result = DialogResult.OK Then
            mFolder = OpenFileDialog1.FileName
        End If
        txtImageDirectory.Text = ""

        'IMAGE WITH HOUGH
        Dim xxx As New MAINCLC
        Dim CRA_DLL As New el_crack
        Dim imgAFT As Bitmap

        imgAFT = CRA_DLL.crack_det(mFolder)
        Pic6.Image = imgAFT


        'GENERAL RECOGNIZING MAINCLC
        Dim dict As Dictionary(Of String, Double)
        dict = xxx.MAIN(mFolder)
        For Each pair As KeyValuePair(Of String, Double) In dict
            txtImageDirectory.Text += (pair.Key & "  -  " & pair.Value) & vbCrLf
        Next


        'CRACK DICTIONARY
        Dim dict_CRACK As New Dictionary(Of String, Double)
        dict_CRACK = CRA_DLL.crack_detect_dict_out(mFolder)
        For Each pair As KeyValuePair(Of String, Double) In dict_CRACK
            txtImageDirectory.Text += (pair.Key & "  -  " & pair.Value) & vbCrLf
        Next


    End Sub

    Private Sub Local_RUN_no_dll_Click(sender As Object, e As EventArgs) Handles Local_RUN_no_dll.Click
        'Load the image from file
        Dim L_a As Integer = 0
        Dim L_b As Integer = 0
        Dim L_c As Integer = 0
        Dim S_a As Integer = 0
        Dim S_b As Integer = 0
        Dim S_c As Integer = 0

        Dim Tot_a As Integer = 0
        Dim Tot_b As Integer = 0
        Dim Tot_c As Integer = 0


        Dim result As DialogResult = OpenFileDialog1.ShowDialog()
        If result = DialogResult.OK Then
            mFolder = OpenFileDialog1.FileName
        End If
        'Dim name As String = OpenFileDialog1.FileName
        Dim img As New Image(Of Bgr, Byte)(mFolder)     'INITIAL IMAGE


        Dim imgnormala As Image(Of Bgr, Byte) = img.Erode(5)

        ''Bilatral filtering
        Dim kernel_size As Integer = 10
        Dim sigma_color As Integer = 50
        Dim sigma_spatial As Integer = 200
        Dim imgBilat As Image(Of Bgr, Byte) = imgnormala.SmoothBilatral(kernel_size, sigma_color, sigma_spatial)   'Bilatral filtering
        ''
        ''segmentation
        Dim imgBILAT_TO_LAPLAS As Image(Of Bgr, Single) = imgBilat.Convert(Of Bgr, Single)
        Dim img_SOBEL As Image(Of Bgr, Single) = imgBilat.Sobel(1, 0, 3)
        Dim img_LAPLAS As Image(Of Bgr, Single) = img_SOBEL.Laplace(17)

        '  Dim img_Subtract As Image(Of Bgr, Single) = img_LAPLAS.Sub(img_SOBEL)
        Dim img_Subtract1 As Image(Of Bgr, Byte) = img_SOBEL.Convert(Of Bgr, Byte)
        Dim img_Subtract2 As Image(Of Bgr, Byte) = img_LAPLAS.Convert(Of Bgr, Byte)
        Dim img_Subtract As Image(Of Bgr, Byte) = img_Subtract1.Sub(img_Subtract2)


        ''
        ''CONTRAST
        Dim imgLAPLAS_TOCONTRAST As Image(Of Gray, Byte) = imgBilat.Convert(Of Gray, Byte)
        Dim img_CONTRAST As Image(Of Gray, Byte) = EqualizeHist(imgLAPLAS_TOCONTRAST)
        ''
        Dim imgCONVERT_TO_OUT As Image(Of Bgr, Byte) = img_CONTRAST.Convert(Of Bgr, Byte)

        ''double color 
        'Dim img_BINARY As Image(Of Bgr, Byte) = imgCONVERT_TO_OUT.ThresholdBinary(New Bgr(100, 100, 0), New Bgr(0, 100, 100)) '(New Bgr(50, 150, 150), New Bgr(150, 150, 20))
        ''
        ''Canny filtering                       'imgCONVERT_TO_OUT
        Dim imgCANNY As Image(Of Gray, Byte) = img_Subtract.Canny(90, 70)  'img.Canny(100, 60) or (80, 50) - best  'last Canny(70, 70) 
        ''

        ''Hough filtering
        Dim RhoRes As Double = 2             '5
        Dim Threshold As Double = 100          '60
        Dim MinLineWidth As Double = 60     '20 - best
        Dim linegap As Integer = 30        '20 - best
        Dim ThetaRes As Double = Math.PI / 180.0
        Dim Linez()() As LineSegment2D = imgCANNY.HoughLinesBinary(RhoRes, ThetaRes, Threshold, MinLineWidth, linegap)
        ''
        ''Line drawing
        If Linez(0).Length >= 0 Then 'Greater than or equal to
            For i As Integer = 0 To Linez(0).Length - 1
                If Linez(0)(i).Length >= 30 And Linez(0)(i).Length < 80 Then 'Greater than and less than or equal to
                    img.Draw(Linez(0)(i), New Bgr(20, 100, 200), 2) 'New Gray(60), 1) '
                    Tot_a = Tot_a + 1
                End If
                If Linez(0)(i).Length >= 120 And Linez(0)(i).Length < 150 Then 'Greater than and less than or equal to
                    img.Draw(Linez(0)(i), New Bgr(255, 0, 0), 4) 'New Gray(100), 2) '
                    Tot_b = Tot_b + 1
                End If
                If Linez(0)(i).Length >= 150 Then
                    img.Draw(Linez(0)(i), New Bgr(0, 0, 255), 4) 'New Gray(150), 3) '
                    Tot_c = Tot_c + 1
                End If
            Next
        End If


        Dim img_OUT As Bitmap = img.ToBitmap


        Dim ResolutBig As Integer = 833658
        Dim colorList_A As New List(Of System.Drawing.Color)
        Dim groups_A = colorList_A.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
        Dim grp_A As IGrouping(Of Color, Color)

        Dim colorList_B As New List(Of System.Drawing.Color)
        Dim groups_B = colorList_B.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
        Dim grp_B As IGrouping(Of Color, Color)

        Dim colorList_C As New List(Of System.Drawing.Color)
        Dim groups_C = colorList_C.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
        Dim grp_C As IGrouping(Of Color, Color)


        For x As Integer = 100 To 300
            For y As Integer = 55 To 995
                colorList_A.Add(img_OUT.GetPixel(x, y))
                img_OUT.SetPixel(x, y, Color.Green)
            Next
        Next
        For x As Integer = 440 To 650
            For y As Integer = 55 To 995
                colorList_B.Add(img_OUT.GetPixel(x, y))
                img_OUT.SetPixel(x, y, Color.Green)
            Next
        Next
        For x As Integer = 790 To 990
            For y As Integer = 55 To 995
                colorList_C.Add(img_OUT.GetPixel(x, y))
                img_OUT.SetPixel(x, y, Color.Green)
            Next
        Next
        'A
        For Each grp_A In groups_A
            If Convert.ToInt32(grp_A(0).R) = 0 And Convert.ToInt32(grp_A(0).G) = 0 And Convert.ToInt32(grp_A(0).B) = 255 Then
                S_a += grp_A.Count      ' short cracks
            End If
            If Convert.ToInt32(grp_A(0).R) = 255 And Convert.ToInt32(grp_A(0).G) = 0 And Convert.ToInt32(grp_A(0).B) = 0 Then
                L_a += grp_A.Count     'long ckracs
            End If
        Next
        'B
        For Each grp_B In groups_B
            If Convert.ToInt32(grp_B(0).R) = 0 And Convert.ToInt32(grp_B(0).G) = 0 And Convert.ToInt32(grp_B(0).B) = 255 Then
                S_b += grp_B.Count      ' short cracks
            End If
            If Convert.ToInt32(grp_B(0).R) = 255 And Convert.ToInt32(grp_B(0).G) = 0 And Convert.ToInt32(grp_B(0).B) = 0 Then
                L_b += grp_B.Count     'long ckracs
            End If
        Next
        'C
        For Each grp_C In groups_C
            If Convert.ToInt32(grp_C(0).R) = 0 And Convert.ToInt32(grp_C(0).G) = 0 And Convert.ToInt32(grp_C(0).B) = 255 Then
                S_c += grp_C.Count      ' short cracks
            End If
            If Convert.ToInt32(grp_C(0).R) = 255 And Convert.ToInt32(grp_C(0).G) = 0 And Convert.ToInt32(grp_C(0).B) = 0 Then
                L_c += grp_C.Count     'long ckracs
            End If
        Next

        Application.DoEvents()

        txtImageDirectory.Text = Tot_a & vbTab & Tot_b & vbTab & Tot_c & vbCrLf & vbCrLf & "|Short A/Long A|" & vbCrLf & S_a & vbTab & L_a & vbCrLf & "|Short B/Long B|" & vbCrLf & S_b & vbTab & L_b & vbCrLf & "Short C/Long C" & vbCrLf & S_c & vbTab & L_c

        ''
        ''Out
        Pic1.Image = imgCANNY.ToBitmap              'FOR INITIAL                 'UI.ImageViewer.Show(img)
        Pic2.Image = img.ToBitmap                   'FOR CANNY
        Pic3.Image = img_OUT '.ToBitmap             'FOR HOUGH

        Pic4.Image = imgBilat.ToBitmap              'Bilatral filtering
        Pic5.Image = imgCONVERT_TO_OUT.ToBitmap     'CONTRAST
        Pic6.Image = img_LAPLAS.ToBitmap            'LAPLAS filtering
        Pic7.Image = img_SOBEL.ToBitmap             'SOBEL filtering
        Pic8.Image = img_Subtract.ToBitmap          '.imgnormala.ToBitmap    erode


    End Sub
    ''more contrast
    Public Shared Function EqualizeHist(ByVal input As Image(Of Gray, Byte)) As Image(Of Gray, Byte)
        Dim output As Image(Of Gray, Byte) = New Image(Of Gray, Byte)(input.Width, input.Height)
        CvInvoke.cvEqualizeHist(input.Ptr, output.Ptr)
        Return output
    End Function

    Private Sub JUST_CRACK_image_Click(sender As Object, e As EventArgs) Handles JUST_CRACK_image.Click
        Dim result As DialogResult = OpenFileDialog1.ShowDialog()
        If result = DialogResult.OK Then
            mFolder = OpenFileDialog1.FileName
        End If

        Dim CRA_DLL As New el_crack
        Dim imgAFT As Bitmap
        imgAFT = CRA_DLL.crack_det(mFolder)
        Pic6.Image = imgAFT
    End Sub
End Class
