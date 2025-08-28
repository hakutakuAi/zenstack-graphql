import { FileWriter } from '@utils/file-writer'
import { OutputFormat } from '@utils/constants'
import path from 'path'

export class HelperFileWriter {
	private fileWriter = new FileWriter()

	async writeHelperFiles(_outputFormat: OutputFormat, outputPath: string, helperCode?: string): Promise<string[]> {
		const files: string[] = []

		if (helperCode) {
			const helperPath = this.resolveHelperPath(outputPath)
			const completeHelperCode = this.combineHelperCode(helperCode)
			await this.fileWriter.write(completeHelperCode, helperPath, 'Helper utilities')
			files.push(helperPath)
		}

		return files
	}

	private resolveHelperPath(basePath: string): string {
		const baseDir = path.dirname(basePath)
		const baseName = path.basename(basePath, path.extname(basePath))

		return path.join(baseDir, `${baseName}-helpers.ts`)
	}

	private combineHelperCode(helperCode: string): string {
		return helperCode
	}
}
