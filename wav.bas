'Option Explicit

'------------------  Constantes ------------------
Const SAMPLE_RATE = 44100          ' Hz
Const VOLUME      = 3000           ' amplitude
Const DUR_SEC     = 0.30           ' duração da nota (s)
Const SIL_SEC     = 0.05           ' pausa entre notas (s)
Const PI          = 3.1415926535897932

'------------------  Utilidades ------------------
Function MidiToFreq(m As Integer) As Double
    Return 440.0 * 2.0 ^ ((m - 69) / 12.0)
End Function

Function LetterToMidi(c As String) As Integer
    Select Case UCase(c)
        Case "A": Return 69
        Case "B": Return 71
        Case "C": Return 60
        Case "D": Return 62
        Case "E": Return 64
        Case "F": Return 65
        Case "G": Return 67
        Case "H": Return 70
        Case Else: Return 0
    End Select
End Function

'------------------  Acordes 0‑9 ------------------
Function GetChord(ch As String, notes() As Integer) As Integer
    ' notes() tem de ter 3 posições (0..2)
    Select Case ch
        Case "0": notes(0)=60 : notes(1)=64 : notes(2)=67 : Return 3
        Case "1": notes(0)=62 : notes(1)=65 : notes(2)=69 : Return 3
        Case "2": notes(0)=64 : notes(1)=67 : notes(2)=71 : Return 3
        Case "3": notes(0)=65 : notes(1)=69 : notes(2)=72 : Return 3
        Case "4": notes(0)=67 : notes(1)=71 : notes(2)=74 : Return 3
        Case "5": notes(0)=69 : notes(1)=72 : notes(2)=76 : Return 3
        Case "6": notes(0)=71 : notes(1)=74 : notes(2)=77 : Return 3
        Case "7": notes(0)=72 : notes(1)=76 : notes(2)=79 : Return 3
        Case "8": notes(0)=74 : notes(1)=77 : notes(2)=81 : Return 3
        Case "9": notes(0)=76 : notes(1)=79 : notes(2)=83 : Return 3
        Case Else: Return 0
    End Select
End Function

'------------------  Cabeçalho WAV ------------------
Sub WriteWavHeader(f As Integer, totalSamples As UInteger)
    Dim As UInteger dataSize   = totalSamples * 2        ' 16‑bit mono
    Dim As UInteger chunkSize  = 36 + dataSize
    Dim As UInteger sub1Size   = 16
    Dim As UShort   audioFmt   = 1                       ' PCM
    Dim As UShort   channels   = 1                       ' mono
    Dim As UShort   bitsPS     = 16
    Dim As UInteger byteRate   = SAMPLE_RATE * channels * bitsPS / 8
    Dim As UShort   blockAlign = channels * bitsPS / 8
    dim as UInteger e=SAMPLE_RATE
    Put #f,, "RIFF"
    Put #f,, chunkSize
    Put #f,, "WAVE"
    Put #f,, "fmt "
    Put #f,, sub1Size
    Put #f,, audioFmt
    Put #f,, channels
    Put #f,, e
    Put #f,, byteRate
    Put #f,, blockAlign
    Put #f,, bitsPS
    Put #f,, "data"
    Put #f,, dataSize
End Sub

'------------------  Programa principal ------------------
Dim As String nomeTxt
color 0,6
cls
Print "Nome do ficheiro de texto a converter: ";
Line Input nomeTxt

Dim As Integer fin = FreeFile
If Open(nomeTxt For Input As #fin) <> 0 Then
    Print "❌ Erro ao abrir "; nomeTxt : End
End If

Dim As Integer fout = FreeFile
If Open("output.wav" For Binary As #fout) <> 0 Then
    Print "❌ Não consegui criar output.wav"
    Close #fin : End
End If

' reserva 44 bytes p/ cabeçalho
For i As Integer = 1 To 44
    Put #fout,, Chr(0)
Next

Dim As Integer noteSamples    = DUR_SEC * SAMPLE_RATE
Dim As Integer silenceSamples = SIL_SEC * SAMPLE_RATE
Dim As UInteger totalSamples  = 0

Dim As String linha, ch
While Not Eof(fin)
    Line Input #fin, linha
    For i As Integer = 1 To Len(linha)
        ch = Mid(linha, i, 1)
        If ch = Chr(10) Or ch = Chr(13) Then Continue For

        Dim As Integer notas(2)
        Dim As Integer n = 0

        If ch >= "0" AndAlso ch <= "9" Then
            n = GetChord(ch, notas())
        Else
            Dim As Integer nota = LetterToMidi(ch)
            If nota <> 0 Then notas(0) = nota : n = 1
        End If

        If n > 0 Then
            ' nota/acorde
            For s As Integer = 0 To noteSamples - 1
                Dim As Double t = s / SAMPLE_RATE
                Dim As Double sample = 0
                For j As Integer = 0 To n - 1
                    sample += Sin(2 * PI * MidiToFreq(notas(j)) * t)
                Next
                sample /= n
                Dim As Short sampleVal = sample * VOLUME
                Put #fout,, sampleVal
            Next

            ' pausa
            Dim As Short zero = 0
            For s As Integer = 0 To silenceSamples - 1
                Put #fout,, zero
            Next

            totalSamples += noteSamples + silenceSamples
        End If
    Next
Wend

Close #fin
Seek #fout, 1
WriteWavHeader(fout, totalSamples)
Close #fout

Print "✅ WAV gerado: output.wav  ("; totalSamples; " samples)"
