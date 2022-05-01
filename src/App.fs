module App

open Browser.Dom
open Browser.Types
open Fable.Core.JsInterop

let canvas = document.getElementById("canvas") :?> HTMLCanvasElement
let context = canvas.getContext_2d()

let maximiseCanvas (canvas: HTMLCanvasElement) =
    let maxX = window.innerWidth - 5.0
    let maxY = window.innerHeight - 5.0
    canvas.width <- maxX
    canvas.height <- maxY

let rec render (ctx: CanvasRenderingContext2D) =
    ctx.strokeStyle <- !^"white"
    ctx.fillStyle <- !^"white"
    ctx.clearRect(0.,0.,ctx.canvas.width, ctx.canvas.height)
    let clockRadius = ((min ctx.canvas.height ctx.canvas.width) / 2.0) - 5.0
    let angleOffset = -System.Math.PI / 2.
    let cx = ctx.canvas.width / 2.
    let cy = ctx.canvas.height / 2.

    // Numerals
    let numeralsRadius = clockRadius - 55.0
    ctx.save()
    ctx.translate(cx, cy)
    ctx.beginPath()
    ctx.font <- "30px serif"
    let numbers = [ 1..12 ] |> List.map(string)
    for text in numbers do
        ctx.rotate(System.Math.PI/6.)
        let textWidth = ctx.measureText(text).width
        ctx.fillText(text, - (textWidth/2.0), - (numeralsRadius + 10.))
    ctx.stroke()
    ctx.restore()

    // Graduations
    let outerRadius = clockRadius
    let innerRadius = outerRadius - 10.
    ctx.save()
    ctx.translate(cx, cy)
    ctx.beginPath()
    ctx.arc(0.,0., outerRadius, 0., System.Math.PI*2.)
    ctx.stroke()
    ctx.beginPath()
    ctx.arc(0.,0., innerRadius, 0., System.Math.PI*2.)
    ctx.stroke()
    for i = 0 to 59 do
        let angle = angleOffset + ((System.Math.PI*2./60.) * (float i))
        if (i) % 15 = 0 then
            ctx.lineWidth <- 1.
            let r = (outerRadius + innerRadius) / 2.
            let x = r * cos angle
            let y = r * sin angle
            ctx.beginPath()
            ctx.moveTo(x,y)
            ctx.arc(x, y, (outerRadius - innerRadius) / 2., 0., System.Math.PI*2.0)
            ctx.fill()
        else            
            if (i) % 5 = 0 then
                ctx.lineWidth <- 4.
            else
                ctx.lineWidth <- 1.
            ctx.beginPath()
            ctx.arc(0.,0., outerRadius, angle, angle)
            ctx.arc(0.,0., innerRadius, angle, angle)
            ctx.stroke()
    ctx.restore()

    // Get current time
    let now = System.DateTime.Now
    let hour = now.Hour
    let minute = now.Minute
    let second = now.Second

    // Hands
    let hourAngle =
        angleOffset +
            (((2.*System.Math.PI)/12.) *
                ((float (if hour <= 12 then hour else hour - 12)) +
                 ((float minute)/60.)))
    let minuteAngle =
        angleOffset +
            (((2.*System.Math.PI)/60.)
                * ((float minute) +
                    ((float second)/60.)))
    let secondAngle = angleOffset + (((2.*System.Math.PI)/60.) * float second)

    let minuteRadius = innerRadius - 5.0
    let hourRadius = numeralsRadius
    let secondRadius = outerRadius 
 
    ctx.save()
    ctx.translate(cx, cy)
    ctx.beginPath()
    ctx.moveTo(0., 0.)
    ctx.lineTo(hourRadius * cos hourAngle, hourRadius * sin hourAngle)
    ctx.moveTo(0., 0.)
    ctx.lineTo(minuteRadius * cos minuteAngle, minuteRadius * sin minuteAngle)
    ctx.moveTo(0., 0.)
    ctx.lineTo(secondRadius * cos secondAngle, secondRadius * sin secondAngle)
    ctx.stroke()
    ctx.restore()

    window.requestAnimationFrame (fun _ -> render ctx) |> ignore

// Maximise canvas size to window size, add event listener to do it every time window is resized
maximiseCanvas canvas
window.addEventListener("resize", fun _ -> maximiseCanvas canvas)

// Begin animating
window.requestAnimationFrame(fun _ -> render context) |> ignore